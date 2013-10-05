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
> POST http://[agent\_host][:port]/api/channels
> Request Params: 
>   required: user, host
>   optional: port, password | ssh\_key
> Response:
>   channel\_id, token

* send command
> PUT http://[agent\_host][:port]/api/channels/$channel\_id
> Request Params:
>   required: channel\_id, token, command\_line\_string
> Response:
>   PID, PPID

* get out/err
> ws://[agent\_host][:port]/websocket
> Request Params:
>   required: channel\_id, token, command\_line\_string
>   optional: line\_no( empty means `from now` )
> Response: ...
