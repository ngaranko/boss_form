-module(test_public_controller, [Req]).
-compile(export_all).


login('GET', []) ->
    Form = boss_form:new(login_form, []),
    {ok, [{form, Form}]};

login('POST', []) ->
    Form = boss_form:new(login_form, []),
    case boss_form:validate(Form, Req:post_params()) of
        ok ->
            %% Do log-in user here
            %...
            {redirect, {controller, account}, {action, index}};
        {error, FormWithErrors} ->
            {ok, [{form, FormWithErrors}]}
    end.