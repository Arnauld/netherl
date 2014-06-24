-define(fail(What),
	begin
	((fun () ->
	    erlang:error({assertion_failed,
				     [{module, ?MODULE},
				      {line, ?LINE},
				      {message, ??What}]})
	  end)())
	end).