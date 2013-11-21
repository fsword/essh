# EasyCMD Agent Design

## ssh console

### aarchitecture

> global

      nginx -> erlang cluster -> machine nodes

> erlang cluster structure


                                           (ssh)
                        api -------> agent ------- machine node
                                       |
      +---------------+                |
      |Auth DB(mnesia)| <-------------- update
      +---------------+

> worker/supervisor tree(module diagram)


### capacity
* active clients:     200                  
* online machines:    600              # clients  * 3
* records:            3K~12K   / day   # machines * 5~20
* lines:              300K~6M  / day   # records  * 100~500
* total(byte):        6M~600M  / day   # lines    * 20~100
* query:              10M      / day

> conclusion
> * active data should be achieved everyday
> * use redis as dynamic store
> * use mongodb/mnesia as document store

### interface

#### agent api

* create channel
> URI:
>   POST http://[agent\_host][:port]/api/channels
> Description:
>   
> Request Params: 
>   required: user, host
>   optional: port, password | ssh\_key
> Response:
>   channel\_id, token

* send command
> URI:
>   PUT http://[agent\_host][:port]/api/channels/$channel\_id
> Description:
>   put the command to command queue of the channel, and agent
> will execute every command on queue when machine is normal.
>   each command is execute over ssh, all stdout/stderr will be
> saved on redis store.
> 
> Request Params:
>   required: channel\_id, token, command
> Response:
>   CommandId

* get out/err(common)
> URI:
>   GET http://[agent\_host][:port]/api/commands/$command\_id
> Description:
>   get the stdout/stderr of specified command. When command is not
> finished, this action will return current output.
> 
> Request Params:
>   required: command\_id, channel\_id, token
> Response:
>   stdout/stderr as body

* get out/err(websocket)
> URI:
>   ws://[agent\_host][:port]/websocket
> Description:
>   return stdout/stderr like stream.
> 
> Request Params:
>   required: command\_id, channel\_id, token
>   optional: line\_no( empty means `from now` )
> Response:
>   stdout/stderr as stream

### supervisor/worker tree

                                 agent_cowboy_app
                                     /    \
                                    /      \
                        agent_ssh_sup       agent_cowboy_sup
                          /   |     \
                         /    |      \
                        /     |       \
                       /      |        \
           agent_channel  agent_id_gen  agent_client_sup
                                             |
                                             |
                                        agent_client

### module design

#### agent\_cowboy\_app
This module is based on cowboy. It designed as global router.

routes:
    "/api/[...]         ->  api_handler
    "/websocket/[...]"  ->  ws_handler

#### handlers 

* api\_handler: used to add web access for the agent.
* ws\_handler:  used to add websocket support of web pages.

#### agent\_channel

This module managed all channels information.  

Channel information is a mapping like:

    {user,host} -> {channelid,token}

#### agent\_id\_gen

This module make global id. 

It used for generate unique key like sequence id of database.

It is based on process dict.

#### agent\_client

This module managed ssh clients.

#### store\_service

This module is designed to supply a common api for storage( eg: redis, mnesia )

### agent client 

As a FSM module, agent client will run according the state diagram like:

          (connected)            (stop)
     new ------------>  normal ----------> terminated
                        |    ∧                 ∧
                (hold)  |    | (continue)      | (stop)
                        v    |                 |
                        paused ----------------


Other commands that agent client can take:

* add cmd: add a command to be execute whatever it has executing command
* result: get the result for the command
* clear: clear all rest commands of the client
* interrupt: kill the command which is executing on the client
