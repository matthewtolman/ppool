%% ----------------------------------
%%
%% @author Matthew Tolman
%% @copyright 2024 Matthew Tolman
%% @doc
%% Plural -> Singular inflection algorithm.
%%
%% Note: This is not a complete algorithm. It's meant for common use cases.
%% It follows the Ruby on Rails algorithm pretty closely, though there is some deviation (e.g. bases -> base vs bases -> basis).
%% @end
%% @version 0.1.1
%% @end
%%
%% ----------------------------------
-module(ppool_inflection).

-export([singular/1]).

-define(PEOPLE_RULE, "people$").
-define(PEOPLE_REPLACE, "person").
-define(MEN_RULE, "men$").
-define(MEN_REPLACE, "man").
-define(CHILD_RULE, "(child)ren$").
-define(CHILD_REPLACE, "\\1").
-define(QUIZ_RULE, "(quiz)es$").
-define(QUIZ_REPLACE, "\\1").
-define(INDEX_RULE, "(vert|ind)ices$").
-define(INDEX_REPLACE, "\\1ex").
-define(MATRIX_RULE, "(matr)ices$").
-define(MATRIX_REPLACE, "\\1ix").
-define(OX_RULE, "(ox)en$").
-define(OX_REPLACE, "\\1").
-define(OCTO_RULE, "(octop|vir)(i|us)$").
-define(OCTO_REPLACE, "\\1us").
-define(CRISIS_RULE, "(cris|test)(is|es)$").
-define(CRISIS_REPLACE, "\\1is").
-define(SHOES_RULE, "(shoe)s$").
-define(SHOES_REPLACE, "\\1").
-define(AXIS_RULE, "(ax)[ie]s$").
-define(AXIS_REPLACE, "\\1is").
-define(BUS_RULE, "(bus)(es)?$").
-define(BUS_REPLACE, "\\1").
-define(OES_RULE, "(o)es$").
-define(OES_REPLACE, "\\1").
-define(MICE_RULE, "(m|l)ice$").
-define(MICE_REPLACE, "\\1ouse").
-define(ES_RULE, "(x|ch|ss|sh)es$").
-define(ES_REPLACE, "\\1").
-define(IES_RULE, "([^aeiouy]|qu)ies$").
-define(IES_REPLACE, "\\1y").
-define(SERIES_RULE, "(series)$").
-define(SERIES_REPLACE, "\\1").
-define(ZOMBIE_RULE, "(zombie|movie)s$").
-define(ZOMBIE_REPLACE, "\\1").
-define(S_RULE, "s$").
-define(S_REPLACE, "").
-define(VES_RULE, "([lr])ves$").
-define(VES_REPLACE, "\\1f").
-define(SIS_RULE, "(analy|diagno|parenthe|progno|synop|the)(sis|ses)$").
-define(SIS_REPLACE, "\\1sis").
-define(UM_RULE, "([ti])a$").
-define(UM_REPLACE, "\\1um").
-define(EWS_RULE, "(news)$").
-define(EWS_REPLACE, "\\1").
-define(SS_RULE, "(ss)$").
-define(SS_REPLACE, "\\1").
-define(GOOSE_RULE, "(geese)$").
-define(GOOSE_REPLACE, "goose").

%% @doc Converts a plural word into a singular word
singular(Word) ->
    singular(Word, [
                    {?SS_RULE, ?SS_REPLACE},
                    {?AXIS_RULE, ?AXIS_REPLACE},
                    {?SHOES_RULE, ?SHOES_REPLACE},
                    {?ES_RULE, ?ES_REPLACE},
                    {?MICE_RULE, ?MICE_REPLACE},
                    {?BUS_RULE, ?BUS_REPLACE},
                    {?OES_RULE, ?OES_REPLACE},
                    {?QUIZ_RULE, ?QUIZ_REPLACE},
                    {?PEOPLE_RULE, ?PEOPLE_REPLACE},
                    {?MEN_RULE, ?MEN_REPLACE},
                    {?CRISIS_RULE, ?CRISIS_REPLACE},
                    {?CHILD_RULE, ?CHILD_REPLACE},
                    {?MATRIX_RULE, ?MATRIX_REPLACE},
                    {?INDEX_RULE, ?INDEX_REPLACE},
                    {?OX_RULE, ?OX_REPLACE},
                    {?OCTO_RULE, ?OCTO_REPLACE},
                    {?SERIES_RULE, ?SERIES_REPLACE},
                    {?ZOMBIE_RULE, ?ZOMBIE_REPLACE},
                    {?VES_RULE, ?VES_REPLACE},
                    {?IES_RULE, ?IES_REPLACE},
                    {?UM_RULE, ?UM_REPLACE},
                    {?SIS_RULE, ?SIS_REPLACE},
                    {?EWS_RULE, ?EWS_REPLACE},
                    {?S_RULE, ?S_REPLACE},
                    {?GOOSE_RULE, ?GOOSE_REPLACE}
                   ]).

singular(Word, []) -> iolist_to_binary([Word]);
singular(Word, [{Rule, Replace} | Rest]) ->
    {ok, Regex} = re:compile(Rule, [caseless]),
    case re:run(Word, Regex) of
        nomatch -> singular(Word, Rest);
        _ -> iolist_to_binary(re:replace(Word, Regex, Replace))
    end.

