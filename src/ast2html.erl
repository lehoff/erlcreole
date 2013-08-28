%% Author: ctoh001
%% Created: 17 Sep 2009
%% Description: TODO: Add description to ast2html
-module(ast2html).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([to_html/1,  to_file/2]).

-export([test/1]).

%%
%% API Functions
%%

%%
%% TODO: Add description of file2html/function_arity
%%
%% to_html(Ast) ->
%% 	HtmlTerm = to_term(Ast),
%% 	element_to_html(HtmlTerm).
%%	xmerl:export_simple([HtmlTerm],xmerl_html).

%% element_to_html({html,Content}) ->
%% 	tag(html,element_to_html(Content));
%% element_to_html({body,Content}) ->
%% 	tag(body,element_to_html(Content));
%% element_to_html({p,Lines}) ->
%% 	tag(p, [element_to_html(Line) ||  Line <- Lines]).

tag_start(Tag) ->
	"<" ++ atom_to_list(Tag) ++ ">".

tag_end(Tag) ->
	"</" ++ atom_to_list(Tag) ++ ">".

tag(Tag,Content) ->
	tag_start(Tag) ++ "\n" ++ Content ++ "\n" ++ tag_end(Tag). 

%%
%% TODO: Add description of file2term/function_arity
%%
to_html({wikipage,Page}) ->
	lists:flatten(tag(html,to_html(Page)));
to_html({paragraphs,Pars}) ->
	tag(body,[to_html(Par) || Par <- Pars]);
to_html({paragraph,{_,_}=Par}) ->
	tag(p,to_html(Par));
to_html({paragraph,Par}) ->	
	tag(p, lists:flatten([to_html(Line) || Line <- Par]));
to_html({text_line,Line}) ->
	[ to_html(Elements) || Elements <- Line];
to_html({string,String}) ->
	String;
to_html({bold,Elements}) ->
	tag(strong, [ to_html(E) || E <- Elements]);
to_html({ital,Elements}) ->
	tag(em, [ to_html(E) || E <- Elements]);
to_html({list_unord,Elements}) ->
	tag(ul, list_to_html(Elements));
to_html({list_ord,Elements}) ->
	tag(ol, list_to_html(Elements));
to_html({list_element,Content}) ->
	tag(li, list_to_html(Content));
to_html(newline) ->
	"";
to_html(List) when is_list(List) ->
	list_to_html(List);
to_html(Rest) ->
	Rest.

%% list_to_html(Any) ->
%% 	"any";
list_to_html(List) when is_list(List)->
	[ to_html(E) || E <- List];
list_to_html(NotList) ->
	"NotList".

%%
%% TODO: Add description of file2file/function_arity
%%
to_file(Ast, OutFile) -> 
	Html = to_html(Ast),
	{ok,S} = file:open(OutFile,[write]),
	io:format(S, "~s",[Html]),
	file:close(S).



%%
%% Local Functions
%%
%% @spec read_file(Filename) -> String
%% read_file(Filename) ->
%% 	{ok,Bin} = file:read_file(Filename),
%% 	binary_to_list(Bin).

%% test({read,Filename}) ->
%% 	read_file("test/" ++ Filename);
%% test({file2term,Filename}) ->
%% 	CreoleAst = creole:file("test/" ++ Filename),
%% 	to_(CreoleAst);
test({file2html,Filename}) ->
	CreoleAst = creole:file("test/" ++ Filename),
	to_html(CreoleAst);
test({file2file,Filename}) ->
	CreoleAst = creole:file("test/" ++ Filename),
	OutFilename = "test/" ++ Filename ++ ".html",
	to_file(CreoleAst,OutFilename).