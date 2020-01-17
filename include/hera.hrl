%%====================================================================
%% @doc hera constants definitions
%% @end
%%====================================================================
%-export_type([filter_task/0]).
%%====================================================================
%% Common Macros
%%====================================================================

%% Thanks to https://github.com/erszcz
%% Helper macro for declaring children of supervisor
% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%-define(SUPFLAGS(Intensity , Period) , #{strategy  => one_for_one
%    ,                                    intensity => Intensity
%    ,                                    period    => Period
%}).

%-define(CHILD(I, Type) , #{id     => I
%    , start    => {I , start_link , []}
%    , restart  => permanent
%    , shutdown => 5000
%    , type     => Type
%    , modules  => [I]
%}).

%%====================================================================
%% Types
%%====================================================================
-type filter_task() :: #{category=> atom(),
                   data=> list(),
                   filter_fun=> function(),
                   frequency=>number()}.

% -type child() :: undefined | pid().

% -type startchild_err() ::
% already_present 
% | {already_started, child()} 
% | term()