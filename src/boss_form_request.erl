-module(boss_form_request).
-export([field_value/2, field_value/3, file_field_value/2, file_field_value/3]).


%% Get field value from request
field_value(FieldName, RequestData) ->
    field_value(FieldName, RequestData, undefined).

field_value(FieldName, RequestData, DefaultValue) ->
    case proplists:get_value(FieldName, RequestData, proplists:get_value(atom_to_list(FieldName), RequestData)) of
        undefined -> DefaultValue;
        "" -> DefaultValue;
        Other -> Other
    end.

%% Get field value from uploaded files
file_field_value(FieldName, UploadedFiles) ->
    file_field_value(FieldName, UploadedFiles, undefined).

file_field_value(_FieldName, [], []) ->
    [];

file_field_value(_FieldName, [], DefaultValue) ->
    case length(DefaultValue) =:= 1 of
        true ->
            hd(DefaultValue);
        false ->
            DefaultValue
    end;

file_field_value(FieldName, [File | UploadedFiles], DefaultValue) ->
    case FieldName =:= list_to_atom(uploaded_file:field_name(File)) of
        true ->
            file_field_value(FieldName, UploadedFiles, [File | DefaultValue]);
        false ->
            file_field_value(FieldName, UploadedFiles, DefaultValue)
    end.
