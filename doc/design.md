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
> * use client process state to store runtime output
> * use mongodb/mnesia as final storage mechanism

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

                        essh_app
                           |
                           |
            ----------  essh_sup       
           |           /   |   \
       essh_store     /    |    \
                     /     |     \
                    /      |      \
        essh_service  essh_id_gen  essh_client_sup
                                          |
                                          |
                                     agent_client

### functional modules diagram

                                     essh_web_api_handler
                                          |
                                          v
                               ---------essh_service ------> essh_receiver
                              |           |
                              |           v
                              |      essh_client
                              |           |
                              |           v
                              -------> essh_store
### module design

#### essh\_app
This module is based on cowboy. It designed as global router.

routes:
    "/api/[...]         ->  api_handler
    "/websocket/[...]"  ->  ws_handler

#### handlers 

* essh\_web\_api\_handler: used to add web access for the agent.
* essh\_web\_ws\_handler:  used to add websocket support of web pages.

#### essh\_channel

This module managed all channels information.  

Channel information is a mapping like:

    {user,host} -> {channelid,token}

#### essh\_id\_gen

This module make global id. 

It used for generate unique key like sequence id of database.

It is based on process dict.

#### essh\_client

This module managed ssh clients.

#### essh\_store

This module is designed to supply a common api for storage( eg: redis, mnesia )

### essh client 

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
* TODO what about this command? "/bin/bash"
