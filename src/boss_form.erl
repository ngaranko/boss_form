-module(boss_form).
-compile(export_all).


%% Bootstrap new form
new(FormModule, InitialData) ->
    FormModule:new(InitialData, []).


%% Draw html fields
fields(Fields, InitialData) ->
    [field_html(FieldName,
                Options,
                proplists:get_value(FieldName,
                                    InitialData)) || {FieldName, Options} <- Fields].

%% Draw form fields
field_html(FieldName, Options, Value) ->
    FieldType = proplists:get_value(type, Options, char_field),
    apply(boss_form_fields, FieldType, [FieldName, Options, Value]).

%% Validation
validate(Form, RequestData) ->
    FormModule = element(1, Form),
    ProcessedFields = [validate_field(Form,
                                      FieldName,
                                      Options,
                                      RequestData) || {FieldName, Options} <- Form:form_fields()],
    case proplists:is_defined(error, ProcessedFields) of
        true ->
            FormData = update_form_data(Form, RequestData),
            {error, FormModule:new(FormData, [Data || {error, Data} <- ProcessedFields])};
        false ->
            ok
    end.

%% Validate field
validate_field(Form, FieldName, Options, RequestData) ->
    Value = request_field_value(FieldName, RequestData, undefined),
    case validate_required(Options, Value) of
        error -> {error, [FieldName, "This field is required"]};
        ok ->
            case validate_apply_form(Form, FieldName, Options, RequestData) of
                ok -> {ok, FieldName};
                {error, ErrorMessage} -> {error, [FieldName, ErrorMessage]}
            end
    end.

%% Apply form field validation, if any
validate_apply_form(Form, FieldName, Options, RequestData) ->
    FormModule = element(1, Form),
    Exports = FormModule:module_info(exports),
    ValidationFunc = list_to_atom(string:concat("validate_", atom_to_list(FieldName))),
    case proplists:is_defined(ValidationFunc, Exports) of
        true ->
            Form:ValidationFunc(Options, RequestData);
        false ->
            ok
    end.

%% Check if field is required and has request value
validate_required(Options, Value) ->
    case proplists:get_value(required, Options, false) of
        true when Value =:= undefined->
            error;
        true when Value =:= "" ->
            error;
        _ -> 
            ok
    end.

%% update form data from request
update_form_data(Form, RequestData) ->
    %% Collect fields data, and fill it with InitialValue or ""
    %% Fill data from request, if possible
    
    [{FieldName, proplists:get_value(atom_to_list(FieldName),
                                     RequestData,
                                     proplists:get_value(FieldName, Form:data(), ""))} || {FieldName, _} <- Form:form_fields()].

request_field_value(FieldName, RequestData, DefaultValue) ->
    proplists:get_value(atom_to_list(FieldName), RequestData, DefaultValue).