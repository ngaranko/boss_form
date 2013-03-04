boss_form
=========

Simple implementation of django forms for [ChicagoBoss](http://chicagoboss.org/).

This module uses parameterized modules, so won't work with R16 :(

Sample Usage
------------

In order to define new form, just create module

```erlang
-module(login_form, [InitialData, Errors]).
-compile(export_all).

%% define form fields inside form_fields function using list of tuples
%%  in format {fieldname, [list of options as tuples]}
%%  curently available field options are
%%    {type, char_field/password_field}
%%    {name, FieldName :: string()}
%%    {required, true/false} - optional param
%%
%% type and name options are mandatory
form_fields() ->
    [
     {username, [{type, char_field},
                 {label, "Username"},
                 {required, true}]},
     {password, [{type, char_field},
                 {widget, password_input},
                 {label, "Password"},
                 {required, true}]},
     {remember, [{type, boolean_field},
                 {label, "Remember me"}]},
     {role, [{type, choice_field},
             {choices, [{0, "None"}, {1, "Test"}]}]}
    ].


%% data/0, errors/0 and fields/0 are required to be defined so far
%% Proxies
data() ->
    InitialData.

errors() ->
    io:format("Errors: ~p.~n", [InitialData]),
    Errors.

fields() ->
    boss_form:fields(form_fields(), InitialData).

as_table() ->
    boss_form:as_table(form_fields(), InitialData, Errors).


%% Custom validation functions are possible
%% just define *validate_fieldname* function with two params 
%% Options - list of options, defined for this field inside form_fields function
%% Req_Value - value provided by Request
validate_password(_Options, RequestData) ->
    case string:len(proplists:get_value("password", RequestData, "")) > 5 of
        true -> ok;
        false -> {error, "Password is too short"}
    end.

```

Sample controller
-----------------

Sample controller can instantiate form using:

```erlang
Form = boss_form:new(login_form, []),
```

And here is code for validating incoming data

```erlang

case boss_form:validate(Form, Req:post_params()) of
    {ok, CleanedData} ->
        {output, "OK"};  %% This one is dummy output
    {error, FormWithErrors} ->
        {ok, [{form, FormWithErrors}]}
end.

```

Sample html
-----------

```html
<form action="" method="POST">
{% if form.errors %}
  {% for id, error in form.errors %}
    {{ error }}
  {% endfor %}
{% endif %}

{% for field in form.fields %}
  {{ field.label }}: {{ field.html }}<br />
{% endfor %}

<table>
    {{ form.as_table }}
</table>

<input type="submit" value="Login" />
</form>
```

Please review examples directory for code example.