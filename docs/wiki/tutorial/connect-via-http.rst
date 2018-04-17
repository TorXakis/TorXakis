Connect TorXakis with the SUT via HTTP
======================================

**NOTE**: the following is a proposal for defining a new type of connections
with the SUT via HTTP.

Suppose we have the have the following ``TorXakis`` channels and data:

.. code-block:: text

   TYPEDEF Cmd ::= CmdInfo
   ENDDEF

   TYPEDEF Response ::= ResponseInfo { version, buildTime :: String }
   ENDDEF


   CHANDEF ChanDefs ::= C :: Cmd 
                      ; R :: Response
   ENDDEF


We would like to specify how actions of the form

.. code-block:: text

   C ! CmdInfo
   R ? someRes

are translated to and from the SUT.


Below is how one would specify an HTTP connection to the SUT:

.. code-block:: text

   CNECTDEF WebService ::=
       HTTP

       MAP C ! CmdInfo TO GET somehost/info
                       WITH RESPONSE r:
                          R ! responseInfo r.body.version r.body.buildTime

   ENDDEF

In the code block above:

- Actions that are sent from ``TorXakis`` to the SUT are mapped into HTTP
  requests. We have to specify the HTTP verb (GET, POST, PUT, DELETE, etc), the
  path, and possibly the request headers, body, and payload. All this
  information has to be extracted from the action that ``TorXakis`` sends to
  the SUT.
- The response of an HTTP request can also generate an action from the SUT. In
  the example above an action of the form ``r ! d`` will be generated, where
  ``d`` is a ``TorXakis`` value that will be filled in based on the response
  body.
- ``r`` is a response value that is assumed to contain a (JSON) body, with
  fields ``version`` and ``buildTime``.

In the case of server sent events or web-sockets, it can be that a web-service
generates ``TorXakis`` actions without requiring an action being sent to the
SUT. For instance:

.. code-block:: text

   CNECTDEF WebService ::=
       HTTP

       MAP GET somehost/server-sent-events
           WITH RESPONSE r:
               R ! ...
   ENDDEF

In the code above an action on channel ``R`` will be generated every time a new
message is sent from the web-server.


New proposal (less general)
===========================

.. code-block:: text

    CNECTDEF WebService ::=
        HTTP "http://localhost:8080"
    
        Cmd ! CmdInfo ->
           GET "/info"
           WITH RESPONSE STATUS 200 AND BODY body:
               Response ! ResponseInfo (body.version, body.buildTime)
           OTHERWISE:
               Response ! ResponseFailure ("Unexpected status")
    
        Cmd ! CmdLoad n ->
           POST "/session/1/model"
           FILE "${n}" -- This will be parsed in a platform specific way.
           WITH RESPONSE STATUS 201:
               Response ! ResponseSuccess
           OTHERWISE:
               Response ! ResponseFailure ("Unexpected status")
    
        Cmd ! CmdStepper model ->
            POST "/stepper/start/1/${model}"
            WITH RESPONSE STATUS 200:
               Response ! ResponseSuccess
            OTHERWISE:
               Response ! ResponseFailure ("Unexpected status")
    
        Cmd ! CmdStep n ->
            POST "/stepper/step/1/${n}"
            IGNORE RESPONSE STATUS 200:
    
            OTHERWISE:
               Response ! ResponseFailure ("Unexpected status")
    
        -- If the message (msg) cannot be parsed we just ignore it.
        -> "/sse/1/messages" WITH msg:
            -- msg.act.contents[0][0].name and msg.act.contents[0][1][0].cInt
            -- will be casted from JSON to the same type of the arguments.
            Response ! ResponseAction( msg.act.contents[0][0].name -- Casted to a TorXakis String
                                     , msg.act.contents[0][1][0].cInt -- Casted to a TorXakis Int
                                     )
        
    
    ENDDEF                

Limitations:

- We cannot handle arrays.
