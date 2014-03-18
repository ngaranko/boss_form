-module(boss_form).
-compile(export_all).


%% Bootstrap new form
new(FormModule, InitialData) ->
    FormModule:new(InitialData, []).

%% Bootstrap new form
new(FormModule, InitialData, FunTrans) when is_function(FunTrans)->
    FormModule:new(InitialData, [], FunTrans).


%% Draw html fields
fields(Fields, InitialData, Errors) ->
    [[{label, field_label(FieldName, Options)},
      {name, FieldName},
      {type, get_field_type(Options)},
      {value, request_field_value(FieldName, InitialData)},
      {errors, get_field_errors(FieldName, Errors)},
      {html, field_html(FieldName, Options, InitialData)}] || {FieldName, Options} <- Fields].

%% Draw form fields
field_html(FieldName, Options, InitialData) ->
    Value = request_field_value(FieldName, InitialData, proplists:get_value(initial, Options)),
    apply(boss_form_fields, get_field_type(Options), [FieldName, Options, Value]).

%% Draw field label
field_label(FieldName, Options) ->
    io_lib:format("<label for='id_~s'>~s</label>",
                  [atom_to_list(FieldName), proplists:get_value(label, Options, atom_to_list(FieldName))]).

%% Output form as table
as_table(Fields, InitialData, Errors) ->
    form_output("<tr><td>~s</td><td>~s~s</td></tr>", table, Fields, InitialData, Errors).

%% Output form as p
as_p(Fields, InitialData, Errors) ->
    form_output("<p>~s: ~s ~s</p>", p, Fields, InitialData, Errors).

%% Build form output
form_output(Tpl, Type, Fields, InitialData, Errors) ->
    form_output(Tpl, Type, Fields, InitialData, Errors, "").

form_output(_Tpl, _Type, [], _InitialData, _Errors, Html) ->
    Html;
form_output(Tpl, Type, [{FieldName, Options} | Fields], InitialData, Errors, Html) ->
    Result = io_lib:format(Tpl, [field_label(FieldName, Options),
                                 field_html(FieldName, Options, InitialData),
                                 build_help_text(Options, Type)]),
    form_output(Tpl, Type, Fields, InitialData, Errors, io_lib:format("~s~s", [Html, Result])).

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
    validate(Form, RequestData, []).

validate(Form, RequestData, UploadedFiles) ->
    FormModule = element(1, Form),
    ProcessedFields = [validate_field(Form,
                                      FieldName,
                                      Options,
                                      RequestData,
                                      UploadedFiles) || {FieldName, Options} <- Form:form_fields()],
    CleanedData = get_processed_data(ProcessedFields),
    case proplists:is_defined(error, ProcessedFields) of
        true ->
            FormData = update_form_data(Form, get_processed_data(ProcessedFields, RequestData)),
            {error, FormModule:new(FormData, [Data || {error, Data} <- ProcessedFields])};
        false ->
            {ok, CleanedData}
    end.

%% Get cleaned data
get_processed_data(ProcessedFields) ->
    [{FieldName, Value} || {ok, FieldName, Value} <- ProcessedFields].

get_processed_data(ProcessedFields, RequestData) ->
    CleanedData = get_processed_data(ProcessedFields),
    [{list_to_atom(FieldName),
     request_field_value(list_to_atom(FieldName), RequestData, proplists:get_value(list_to_atom(FieldName), CleanedData))} || {FieldName, _} <- RequestData].

%% Validate field
validate_field(Form, FieldName, Options, RequestData) ->
    validate_field(Form, FieldName, Options, RequestData, []).

validate_field(Form, FieldName, Options, RequestData, UploadedFiles) ->
    Value = case proplists:get_value(type, Options, char_field) of
                file_field ->
                    request_file_field_value(FieldName, UploadedFiles, proplists:get_value(initial, Options, []));
                _Other ->
                    request_field_value(FieldName, RequestData, proplists:get_value(initial, Options, undefined))
            end,
    CleanedValue = clean_field(FieldName, Value, Options, proplists:get_value(type, Options, char_field)), %% Validate as string by default
    case CleanedValue of
        {error, ErrorData} ->
            {error, [FieldName, ErrorData]};
        OtherValue ->
            ErrorMessages = proplists:get_value(error_messages, Options, []),
            case validate_required(Options, OtherValue) of
                error ->
                    {error, {FieldName, proplists:get_value(requried, ErrorMessages, "This field is required")}};
                ok ->
                    case validate_min_length(Options, OtherValue) of
                        ok ->
                            case validate_max_length(Options, OtherValue) of
                                ok ->
                                    case validate_apply_form(Form, FieldName, Options, RequestData) of
                                        ok ->
                                            {ok, FieldName, OtherValue};
                                        {ok, ValidValue} ->
                                            {ok, FieldName, ValidValue};
                                        {error, ErrorMessage} ->
                                            {error, {FieldName, ErrorMessage}}
                                    end;
                                {error, Message} ->
                                    {error, {FieldName, proplists:get_value(max_length, ErrorMessages, Message)}}
                            end;
                        {error, Message} ->
                            {error, {FieldName, proplists:get_value(min_length, ErrorMessages, Message)}}
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
        true when Value =:= undefined ->
            error;
        true when Value =:= "" ->
            error;
        _ ->
            ok
    end.

%% Check field min length
validate_min_length(Options, Value) ->
    case proplists:get_value(min_length, Options, false) of
        MinLength when is_integer(MinLength), Value =/= "", Value =/= undefined ->
            case MinLength > string:len(Value) of
                true ->
                    {error, io_lib:format("Field length is less than ~p", [MinLength])};
                false ->
                    ok
            end;
        _ ->
            ok
    end.

%% Check field max lenght
validate_max_length(Options, Value) ->
    case proplists:get_value(max_length, Options, false) of
        MaxLength when is_integer(MaxLength), Value =/= "", Value =/= undefined ->
            case MaxLength < string:len(Value) of
                true ->
                    {error, io_lib:format("Field length is more than ~s", [MaxLength])};
                false ->
                    ok
            end;
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

%% Get field value from uploaded files
request_file_field_value(FieldName, UploadedFiles) ->
    request_file_field_value(FieldName, UploadedFiles, undefined).

request_file_field_value(_FieldName, [], []) ->
    [];
request_file_field_value(_FieldName, [], DefaultValue) ->
    case length(DefaultValue) =:= 1 of
        true ->
            hd(DefaultValue);
        false ->
            DefaultValue
    end;

request_file_field_value(FieldName, [File | UploadedFiles], DefaultValue) ->
    case FieldName =:= list_to_atom(uploaded_file:field_name(File)) of
        true ->
            request_file_field_value(FieldName, UploadedFiles, [File | DefaultValue]);
        false ->
            request_file_field_value(FieldName, UploadedFiles, DefaultValue)
    end.

%% Get field errors from Errors proplist
get_field_errors(FieldName, Errors) ->
    get_field_errors(FieldName, Errors, []).
get_field_errors(FieldName, Errors, DefaultValue) ->
    proplists:get_value(FieldName, Errors, DefaultValue).

%% Get Field type
get_field_type(Options) ->
    proplists:get_value(type, Options, char_field).

%% Update existing model from form
update_model(Model, Data) ->
    update_model(Model, Data, Model:attributes()).

update_model(Model, _Data, []) ->
    Model;
update_model(Model, Data, [{id, _Value} | Attrs]) ->
    update_model(Model, Data, Attrs);
update_model(Model, Data, [{AttrName, Value} | Attrs]) ->
    update_model(Model:set(AttrName, proplists:get_value(AttrName, Data, Value)), Data, Attrs).

%% Build helptext
build_help_text(Options) ->
    build_help_text(Options, table).

build_help_text(Options, table) ->
    io_lib:format("<br />~s", [build_help_text(Options, p)]);
build_help_text(Options, p) ->
    case proplists:get_value(help_text, Options) of
        undefined ->
            "";
        Text ->
            io_lib:format("<span class=\"helptext\">~s</span>", [Text])
    end.
