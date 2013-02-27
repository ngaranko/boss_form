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
                 {name, "Username"},
                 {required, true}]},
     {password, [{type, password_field},
                 {name, "Password"},
                 {required, true}]}
    ].

%% data/0, errors/0 and fields/0 are required to be defined so far
data() ->
    InitialData.

errors() ->
    Errors.

fields() ->
    boss_form:fields(form_fields(), InitialData).

%% Custom validation functions are possible
%% just define *validate_fieldname* function with two params 
%% Options - list of options, defined for this field inside form_fields function
%% Req_Value - value provided by Request
validate_password(_Options, Req_Value) ->
    case string:len(Req_Value) > 5 of
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
    ok ->
        %% Do log-in user here
        %...
        {redirect, {controller, account}, {action, index}};
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
{{ form.fields }}
<input type="submit" value="Login" />
</form>
```

Please review examples directory for code example.