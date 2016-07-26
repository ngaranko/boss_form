boss_form [![Build Status](https://travis-ci.org/ngaranko/boss_form.svg?branch=alpha)](https://travis-ci.org/ngaranko/boss_form)
=========

Simple implementation of django forms for [ChicagoBoss](http://chicagoboss.org/).


Main purpose of this lib is to simplify form creation and processing.

Usage
------

```erlang
% ChicagoBoss controller.
login('GET', [], Context) ->
    Form = boss_form:new(login_form, []),
    {ok, [{form, Form}] ++ Context};

login('POST', [], Context) ->
    Form = boss_form:new(login_form, Req:post_params()),
    case boss_form:is_valid(Form) of
        {ok, CleanedForm} ->
            boss_session:set_session_data(SessionID, username, proplists:get_value(username, CleanedForm#boss_form.cleaned_data)),
            {redirect, "/panel"};
        {error, FormWithErrors} ->
            {ok, [{form, FormWithErrors}] ++ Context}
    end.
```

Example form
--------------

```erlang
-module(test_form).
-compile(export_all).

form_fields() ->
    [
     {username, [{type, char_field},
                 {label, "Username"},
                 {required, true},
                 {min_length, 5},
                 {max_length, 255},
                 {html_options, [{class, "form-control"},
                                {placeholder, "Email address"},
                                {autofocus, autofocus},
                                {required, required}]}]},
     {password, [{type, char_field},
                 {widget, password_input},
                 {label, "Password"},
                 {min_length, 5},
                 {max_length, 25},
                 {required, true},
                 {html_options, [{class, "form-control"},
                                {placeholder, "Password"},
                                {required, required}]}]}
    ].
```

Example template
--------------------

```html
<form class="form-signin" role="form" method="POST">
  {{ csrf_token|safe }}
  <h2 class="form-signin-heading">Please sign in</h2>
  {% for error, text in form.errors %}
    {{ error }}: {{ text }}<br />
  {% endfor %}
  {% for field in form.fields %}
      <div class="{% if field.errors %} has-error{% endif %}">
          {{ field.html|safe }}
      </div>
  {% endfor %}
  <button type="submit">Sign in</button>
</form>

```


Form field definition
---------------------

Fields are defined in form_fields/0 function as proplist in form {field_name, Options}.

Field Options
-------------

Required params:

+ type::atom() - field type, one of: [char_field, float_field, boolean_field, choice_field, file_field ] % please refer to [Field types and Widgets doc](README_FIELDS_AND_WIDGETS.md)
+ label::string() - field label as string

Optional params:

+ required::atom() - true/false Notifies if field is required to be present while checking form contents, defaults to false.
+ initial::term() - Initial value for field
+ widget::tuple() - Custom field widget in form {Module, Function}, note that widget should be saved outside of parametrized modules (i.e. not in form module)
+ html_options::proplist() - List of html properties [{Key::term(), Value::string()}], will be added to output as Key="Value"

Field specific params:

+ float_field can have format::string() param, this format will be used by io_lib to format field value. Default: "~.2f"
+ choice_field can have choices::proplist() param, in form [{value::term(), title::string()}]

Examples
--------

Please see examples/ directory for sample app files.

Todos
-----

- [ ] add most used django field types
- [ ] add most used django widgets
- [ ] add inline documentation
- [ ] add django-like output formats as_p, etc.
- [ ] replace io_lib formatting with dtl
- [ ] get rid of proxy functions inside form definition [data/0, errors/0, fields/0, as_table/0]
