-module(login_form, [InitialData, Errors]).
-compile(export_all).

form_fields() ->
    [
     {username, [{type, char_field},
                 {name, "Username"},
                 {required, true}]},
     {password, [{type, password_field},
                 {name, "Password"},
                 {required, true}]}
    ].

data() ->
    InitialData.

errors() ->
    Errors.

fields() ->
    boss_form:fields(form_fields(), InitialData).


validate_password(_Options, RequestData) ->
    case string:len(proplists:get("password", RequestData, "")) > 5 of
        true -> ok;
        false -> {error, "Password is too short"}
    end.