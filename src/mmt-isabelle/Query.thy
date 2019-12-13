chapter \<open>Isabelle/MMT query within formal context\<close>

theory Query
  imports Main
  keywords "mmt_query" :: diag
begin

section \<open>Isar command\<close>

ML \<open>
local

(* theory content for given terms *)

fun theory_content thy ts =
  let
    fun add_entry space_of name tab =
      if Symtab.defined tab name then tab
      else
        let val space = space_of thy
        in Symtab.update (name, #theory_long_name (Name_Space.the_entry space name)) tab end;

    val add_classes = fold_atyps (Type.sort_of_atyp #> fold (add_entry Sign.class_space));

    fun add_types (Type (c, args)) = add_entry Sign.type_space c #> fold add_types args
      | add_types _ = I;

    val add_consts = fold_aterms (fn Const (c, _) => add_entry Sign.const_space c | _ => I);

    fun make_content add = Symtab.dest (fold add ts Symtab.empty) |> sort_by #1;
  in
   {classes = make_content (fold_types add_classes),
    types = make_content (fold_types add_types),
    consts = make_content add_consts}
  end;

fun encode_content {classes, types, consts} =
  let
    open XML.Encode;
    val entries = list (pair string string);
  in triple entries entries entries (classes, types, consts) end;

in

(* Isar command *)

val _ =
  Outer_Syntax.command \<^command_keyword>\<open>mmt_query\<close> "Isabelle/MMT query"
    (Scan.repeat1 Parse.term >> (fn ss =>
      Toplevel.keep (fn state =>
        let
          val ctxt = Toplevel.context_of state;
          val thy = Toplevel.theory_of state;
          val consts = Sign.consts_of thy;

          val ts = map (Syntax.read_term ctxt) ss;
          val _ =
            Pretty.writeln (Pretty.big_list "query:"
              (map (Pretty.item o single o Syntax.pretty_term ctxt) ts));

          val narration_base = Options.default_string \<^system_option>\<open>mmt_query_narration_base\<close>;

          val result =
            (narration_base, theory_content thy ts, ts)
            |> let open XML.Encode Term_XML.Encode
               in triple string encode_content (list (term consts)) end
            |> YXML.string_of_body
            |> Invoke_Scala.method "info.kwarc.mmt.isabelle.Query.command"
            |> YXML.parse_body
            |> let open XML.Decode Term_XML.Decode in list string end;

          val _ = Pretty.writeln (Pretty.big_list "result:" (map Pretty.str result));
        in () end)));

end
\<close>


section \<open>Examples\<close>

mmt_query
  \<open>\<forall>x y::'a::order. x \<le> y \<longrightarrow> y \<le> x\<close>
  \<open>\<exists>z::'a::lattice. \<forall>x. x \<le> z\<close>

mmt_query "x = y" "x \<le> y"

end
