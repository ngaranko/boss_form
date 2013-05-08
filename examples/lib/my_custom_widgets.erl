-module(my_custom_widgets).
-export([bootstrap_price_input/3]).

bootstrap_price_input(Name, Value, Options) ->
    io_lib:format("<div class=\"input-prepend\"><span class=\"add-on\">$</span>~s</div>",
                  [boss_form_widget:text_input(Name, Value, Options)]).