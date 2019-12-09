theory Query
  imports Pure
  keywords "mmt_query" :: diag
begin

ML \<open>
  Outer_Syntax.command \<^command_keyword>\<open>mmt_query\<close> "Isabelle/MMT query"
    (Scan.repeat1 Parse.term >> (fn raw_query =>
      Toplevel.keep (fn state =>
        let
          val ctxt = Toplevel.context_of state;
          val thy = Toplevel.theory_of state;
          val consts = Sign.consts_of thy;

          val query = map (Syntax.read_term ctxt) raw_query;
          val _ = Pretty.writeln (Pretty.big_list "query:" (map (Syntax.pretty_term ctxt) query));

          val result = query
            |> let open XML.Encode Term_XML.Encode in list (term consts) end
            |> YXML.string_of_body
            |> Invoke_Scala.method "info.kwarc.mmt.isabelle.Query.command"
            |> YXML.parse_body
            |> let open XML.Decode Term_XML.Decode in list (term consts) end;

          val _ = Pretty.writeln (Pretty.big_list "result:" (map (Syntax.pretty_term ctxt) result));
        in () end)))
\<close>

mmt_query "x \<equiv> y" "PROP A \<Longrightarrow> PROP B"

end
