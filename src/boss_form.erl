-module(boss_form).
-compile(export_all).


%% Bootstrap new form
new(FormModule, InitialData) ->
    FormModule:new(InitialData, []).


%% Draw html fields
fields(Fields, InitialData) ->
    [[{label, field_label(FieldName, Options)},
      {name, FieldName},
      {html, field_html(FieldName, Options, InitialData)}] || {FieldName, Options} <- Fields].

%% Draw form fields
field_html(FieldName, Options, InitialData) ->
    Value = request_field_value(FieldName, InitialData, proplists:get_value(default, Options)),
    FieldType = proplists:get_value(type, Options, char_field),
    apply(boss_form_fields, FieldType, [FieldName, Options, Value]).

field_label(FieldName, Options) ->
    io_lib:format("<label for='id_~s'>~s</label>",
                  [atom_to_list(FieldName), proplists:get_value(label, Options, atom_to_list(FieldName))]).

%% Output form as table
as_table(Fields, InitialData, _Errors) ->
    [io_lib:format("<tr><td>~s</td><td>~s</td></tr>",
        [field_label(FieldName, Options),
         field_html(FieldName, Options, InitialData)]) || {FieldName, Options} <- Fields].

%% Clean
clean_field(_FieldName, Value, Options, FieldType) ->
    Cleaner = list_to_atom(string:concat(atom_to_list(FieldType), "_clean")),
    Exports = boss_form_fields:module_info(exports),
    case proplists:get_value(Cleaner, Exports, 1) of
        1 -> apply(boss_form_fields, Cleaner, [Value]);
        2 -> apply(boss_form_fields, Cleaner, [Value, Options])
    end.

%% Validation
validate(Form, RequestData) ->
    FormModule = element(1, Form),
    ProcessedFields = [validate_field(Form,
                                      FieldName,
                                      Options,
                                      RequestData) || {FieldName, Options} <- Form:form_fields()],
    CleanedData = get_processed_data(ProcessedFields),
    case proplists:is_defined(error, ProcessedFields) of
        true ->
            FormData = update_form_data(Form, get_processed_data(ProcessedFields, RequestData)),
            {error, FormModule:new(FormData, [Data || {error, Data} <- ProcessedFields])};
        false ->
            {ok, CleanedData}
    end.

get_processed_data(ProcessedFields) ->
    [{FieldName, Value} || {ok, FieldName, Value} <- ProcessedFields].

get_processed_data(ProcessedFields, RequestData) ->
    CleanedData = get_processed_data(ProcessedFields),
    [{list_to_atom(FieldName),
     request_field_value(list_to_atom(FieldName), RequestData, proplists:get_value(list_to_atom(FieldName), CleanedData))} || {FieldName, _} <- RequestData].

%% Validate field
validate_field(Form, FieldName, Options, RequestData) ->
    Value = request_field_value(FieldName, RequestData, proplists:get_value(default, Options, undefined)),
    CleanedValue = clean_field(FieldName, Value, Options, proplists:get_value(type, Options, char_field)), %% Validate as string by default
    case CleanedValue of
        {error, ErrorData} ->
            {error, [FieldName, ErrorData]};
        OtherValue ->
            case validate_required(Options, OtherValue) of
                error -> {error, [FieldName, "This field is required"]};
                ok ->
                    case validate_apply_form(Form, FieldName, Options, RequestData) of
                        ok -> {ok, FieldName, OtherValue};
                        {ok, ValidValue} -> {ok, FieldName, ValidValue};
                        {error, ErrorMessage} -> {error, [FieldName, ErrorMessage]}
                    end
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
    [{FieldName, proplists:get_value(FieldName,
                                     RequestData,
                                     proplists:get_value(FieldName, Form:data(), ""))} || {FieldName, _} <- Form:form_fields()].

%% Get field value from request
request_field_value(FieldName, RequestData) ->
    request_field_value(FieldName, RequestData, undefined).

request_field_value(FieldName, RequestData, DefaultValue) ->
    case proplists:get_value(FieldName, RequestData, proplists:get_value(atom_to_list(FieldName), RequestData)) of
        undefined -> DefaultValue;
        "" -> DefaultValue;
        Other -> Other
    end.

update_model(Model, Data) ->
    update_model(Model, Data, Model:attributes()).

update_model(Model, _Data, []) ->
    Model;
update_model(Model, Data, [{id, _Value} | Attrs]) ->
    update_model(Model, Data, Attrs);
update_model(Model, Data, [{AttrName, Value} | Attrs]) ->
    update_model(Model:set(AttrName, proplists:get_value(AttrName, Data, Value)), Data, Attrs).
