%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2012, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(ws_handler).  

-behaviour(cowboy_http_handler).  
-behaviour(cowboy_http_websocket_handler).  
  
% Behaviour cowboy_http_handler API
-export([init/3, handle/2, terminate/2]).  
  
% Behaviour cowboy_http_websocket_handler API 
-export([websocket_init/3, websocket_handle/3,  
	 websocket_info/3, websocket_terminate/3]).

-record(state, {room}).
-record(user, {pid, nick}).
-record(message, {sender, text, time}).

%%--------------------------------------------------------------------
%% @doc
%% This function is called to know how to dispatch a new connection.
%%
%% @spec init(_Any, Req, []) -> {ok, Req2, undefined} |
%%                              {upgrade, protocol, cowboy_http_websocket}
%% @end
%%-------------------------------------------------------------------- 
init({tcp, http}, Req, _Opts) ->
    io:format("self ~p~n", [self()]),
    case cowboy_http_req:header('Upgrade', Req) of
	{undefined, Req2} -> {ok, Req2, undefined};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    Page = get_page(),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}], Page, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    lager:debug("~p terminate.", [self()]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This function is called for every new websocket connection.
%%
%% @spec websocket_init(_Any, Req, []) -> {ok, Req2, undefined, hibernate}
%% @end
%%-------------------------------------------------------------------- 
websocket_init(_Any, Req, []) ->
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, #state{}, hibernate}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called when a text message arrives. 
%%
%% @spec websocket_handle(Msg, Req, State) -> {reply, Reply, Req, State, hibernate} |
%%                                            {ok, Req, State}
%% @end
%%-------------------------------------------------------------------- 
websocket_handle({text, <<"join ", Rest/binary>>}, Req, State) ->
    [RoomStr, NickStr] = string:tokens(binary_to_list(Rest), " "),
    Room = list_to_existing_atom(RoomStr),
    Nick = list_to_binary(NickStr),
    User = #user{nick=Nick, pid=self()},
    room:join(Room, User),
    {ok, Req, State#state{room=Room}, hibernate};

websocket_handle({text, <<"msg ", Msg/binary>>}, Req, #state{room=Room} = State) ->
    room:send(Room, self(), Msg),
    {ok, Req, State, hibernate};

websocket_handle({text, <<"leave">>}, Req, #state{room=Room} = _State) ->
    room:leave(Room, self()),
    {ok, Req, #state{}, hibernate};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called when other messages from the system arrives.
%%
%% @spec websocket_info(Info, Req, State) -> {reply, Reply, Req, State, hibernate} |
%%                                           {ok, Req, State, hibernate}
%% @end
%%-------------------------------------------------------------------- 
websocket_info({msg, Msg}, Req, State) ->
    Reply = convert_to_json(Msg),
    lager:debug("~p", [Reply]),
    {reply, {text, Reply}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, #state{room=Room} = _State) ->
    room:leave(Room, self()),
    lager:debug("~p websocket terminate.", [self()]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This function converts into json the message
%%
%% @spec convert_to_json(Actors) -> Json
%% @end
%%-------------------------------------------------------------------- 
convert_to_json(Message) ->
    Data = [{obj,
	     [{text, Message#message.text}, 
	      {sender, Message#message.sender},
	      {time, tuple_to_list(Message#message.time)}]}
	   ],
    JsonData = {obj, [{message, Data}]},
    rfc4627:encode(JsonData).

get_page() ->
    %% HTML code taken from misultin's example file.
    <<"<html>
<head>
<script type=\"text/javascript\">

function addStatus(text){
		    document.getElementById('status').innerHTML
		    = document.getElementById('status').innerHTML
		    + text + \"<br/>\";
}

function addOutput(text){
		    document.getElementById('output').innerHTML
		    = document.getElementById('output').innerHTML
		    + text + \"<br/>\";
}

function ready(){
		if (\"MozWebSocket\" in window) {
		WebSocket = MozWebSocket;
	}
	if (\"WebSocket\" in window) {
		// browser supports websockets
		var ws = new WebSocket(\"ws://localhost:8080/websocket\");
                var offline = true;
		ws.onopen = function() {
			addStatus(\"Websocket connected!\");
		};
		ws.onmessage = function (evt) {
                        var msg = JSON.parse(evt.data);
                        var text = msg.message[0].text;
                        var sender = msg.message[0].sender;
			addOutput(sender + \": \" + text + \"\");
		};
		ws.onclose = function() {
			addStatus(\"websocket was closed!\");
		};
                
		send_msg = function() {
                        var message = document.getElementById('msg');
                        if (! offline) {
			    ws.send('msg' + \" \" + message.value);
			    message.value = '';			    
                        } else {
                            alert('Join chat first');
                        }
                        message.value = '';
		  	return true;
	        };
		join_leave_chat = function() {
                        var mybutton = document.getElementById('mybutton');
                        if (offline) {
			     var nick = document.getElementById('nick');
                             var room = document.getElementById('room');
                             var roomName = room.options[room.selectedIndex].text;
		             ws.send('join' + \" \" + roomName + \" \" + nick.value);
                             mybutton.value = 'Leave';
                             mybutton.style.backgroundColor = 'red';
                             offline = false;
                        } else {
                             ws.send(\"leave\");
                             mybutton.value = 'Join';
                             mybutton.style.backgroundColor = '#fff';
                             offline = true;
                        }
		  	return true;
	        };
	} else {
		addStatus(\"sorry, your browser does not support websockets.\");
	}
}
</script>
    <style>
      body {
        background-color: #ADB04D;
        font-family: 'Ubuntu', arial, serif;
        font-size: 14px;
      }

      #main {
        width: 600px;
        padding: 20px;
        margin: 40px auto;
        background: white;
        -webkit-box-shadow: 0 0 10px rgba(0, 0, 0, 0.3), 0 0 60px rgba(0, 0, 0, 0.1) inset;
        -moz-box-shadow: 0 0 10px rgba(0, 0, 0, 0.3), 0 0 40px rgba(0, 0, 0, 0.1) inset;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.3), 0 0 40px rgba(0, 0, 0, 0.1) inset;
      }

      #output {
        height: 300px;
        width: 100%;
        overflow: auto;
        -moz-border-radius: 3px;
        border-radius: 3px;
        border: 1px solid #aaa;
      }

      #output p {
        padding: 8px;
        margin: 0;
      }

      #output p:nth-child(odd) {
        background: #F6F6F6;
      }

      input, select {
        padding: 5px;
        background: #fff;
        -moz-border-radius: 3px;
        border-radius: 3px;
        -webkit-box-shadow: 0 2px 4px rgba(0, 0, 0, 0.3) inset;
        -moz-box-shadow: 0 2px 4px rgba(0, 0, 0, 0.3) inset;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.3) inset;
        border: none;
        border: 1px solid #aaa;
      }

      h1 {}

      #msg, #output {
        width: 100%;
      }

      #status {
        border: 3px solid #e28c33;
        background-color: #FEE77D;
        padding: 5px 10px;
        font-family: monospace;
      }
    </style>
  </head>
  <body onload=\"ready();\">
    <div id=\"main\">

    <h1>Erlang Websocket Chat</h1>

      <p><label>Nick <input id=\"nick\"></label>
      <label>Pick a room
      <select id=\"room\">
       <option>italian</option>
       <option>english</option>
       <option>french</option>
      </select>
      </label>
      <input type=\"button\" id=\"mybutton\" onclick=\"join_leave_chat(); return false\" value=\"Join\"/>
      </p>

    <div id=\"output\"></div>

      <p><input id=\"msg\" onchange=\"send_msg(); return false;\"></p>

    <div id=\"status\"></div>
    <div>
  </body>
</html>">>.
