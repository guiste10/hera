%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jun 2020 1:00 PM
%%%-------------------------------------------------------------------
-module(hera_utils).
-author("julien").

%% API
-export([concat_atoms/2]).

concat_atoms(Atom1, Atom2) ->
  L = atom_to_list(Atom1) ++ atom_to_list(Atom2),
  list_to_atom(L).