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
