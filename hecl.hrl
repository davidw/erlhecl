%% Interpreter state.  Theoretically, we could have a couple different
%% interpreters running around...

-record(interp,
	{
	  commands = ets:new(commands, [set]),
	  vars = ets:new(commands, [set])
	 }).

-record(thing,
	{
	  type = str,
	  val = undefined
	 }).
