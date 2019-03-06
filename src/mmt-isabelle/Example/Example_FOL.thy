chapter \<open>Example proof document in Isabelle/FOL\<close>

theory Example_FOL
  imports FOL
begin

section \<open>Natural numbers: Peano's axioms, primitive recursion\<close>

typedecl nat
instance nat :: \<open>term\<close> ..

axiomatization Zero :: \<open>nat\<close>  (\<open>0\<close>)
    and Suc :: \<open>nat \<Rightarrow> nat\<close>
    and rec :: \<open>[nat, 'a, [nat, 'a] \<Rightarrow> 'a] \<Rightarrow> 'a\<close>
  where induct: \<open>P(0) \<Longrightarrow> (\<And>x. P(x) \<Longrightarrow> P(Suc(x))) \<Longrightarrow> P(n)\<close>
    and Suc_inject: \<open>Suc(m) = Suc(n) \<Longrightarrow> m = n\<close>
    and Suc_neq_0: \<open>Suc(m) = 0 \<Longrightarrow> R\<close>
    and rec_0: \<open>rec(0, a, f) = a\<close>
    and rec_Suc: \<open>rec(Suc(m), a, f) = f(m, rec(m, a, f))\<close>

definition add :: \<open>[nat, nat] \<Rightarrow> nat\<close>  (infixl \<open>+\<close> 60)
  where \<open>m + n \<equiv> rec(m, n, \<lambda>x y. Suc(y))\<close>

lemma Suc_n_not_n: \<open>Suc(k) \<noteq> k\<close>  (is "?P(k)")
proof (rule induct [where n = k])
  show "?P(0)"
  proof
    assume "Suc(0) = 0"
    then show False by (rule Suc_neq_0)
  qed
  show "?P(Suc(k))" if hyp: "?P(k)" for k
  proof
    assume "Suc(Suc(k)) = Suc(k)"
    then have "Suc(k) = k" by (rule Suc_inject)
    with hyp show False ..
  qed
qed

lemma add_0 [simp]: \<open>0 + n = n\<close>
  unfolding add_def by (rule rec_0)

lemma add_Suc [simp]: \<open>Suc(m) + n = Suc(m + n)\<close>
  unfolding add_def by (rule rec_Suc)

lemma add_assoc: \<open>(k + m) + n = k + (m + n)\<close>
  by (rule induct [where n = k]) simp_all

lemma add_0_right [simp]: \<open>m + 0 = m\<close>
  by (rule induct [where n = m]) simp_all

lemma add_Suc_right [simp]: \<open>m + Suc(n) = Suc(m + n)\<close>
  by (rule induct [where n = m]) simp_all



section \<open>Algebraic structures: locales and interpretations\<close>

locale semigroup =
  fixes op :: "'a \<Rightarrow> 'a \<Rightarrow> 'a"  (infixl "\<bullet>" 70)
  assumes assoc: "(x \<bullet> y) \<bullet> z = x \<bullet> (y \<bullet> z)"
begin

definition dup :: "'a \<Rightarrow> 'a"
  where "dup(x) = x \<bullet> x"

lemma dup_dup: "dup(dup(x)) = x \<bullet> x \<bullet> x \<bullet> x"
  by (simp only: dup_def assoc)

end

locale monoid = semigroup +
  fixes unit :: "'a"  ("\<one>")
  assumes left_unit: "\<one> \<bullet> x = x"
    and right_unit: "x \<bullet> \<one> = x"

locale group = semigroup +
  fixes unit :: "'a"  ("\<one>")
    and inverse :: "'a \<Rightarrow> 'a"  ("(_\<inverse>)" [1000] 999)
  assumes left_unit: "\<one> \<bullet> x = x"
    and left_inverse: "x\<inverse> \<bullet> x = \<one>"
begin

lemma right_inverse: "x \<bullet> x\<inverse> = \<one>"
proof -
  have "x \<bullet> x\<inverse> = \<one> \<bullet> (x \<bullet> x\<inverse>)" by (simp only: left_unit)
  also have "\<dots> = \<one> \<bullet> x \<bullet> x\<inverse>" by (simp only: assoc)
  also have "\<dots> = (x\<inverse>)\<inverse> \<bullet> x\<inverse> \<bullet> x \<bullet> x\<inverse>" by (simp only: left_inverse)
  also have "\<dots> = (x\<inverse>)\<inverse> \<bullet> (x\<inverse> \<bullet> x) \<bullet> x\<inverse>" by (simp only: assoc)
  also have "\<dots> = (x\<inverse>)\<inverse> \<bullet> \<one> \<bullet> x\<inverse>" by (simp only: left_inverse)
  also have "\<dots> = (x\<inverse>)\<inverse> \<bullet> (\<one> \<bullet> x\<inverse>)" by (simp only: assoc)
  also have "\<dots> = (x\<inverse>)\<inverse> \<bullet> x\<inverse>" by (simp only: left_unit)
  also have "\<dots> = \<one>" by (simp only: left_inverse)
  finally show ?thesis .
qed

lemma right_unit: "x \<bullet> \<one> = x"
proof -
  have "x \<bullet> \<one> = x \<bullet> (x\<inverse> \<bullet> x)" by (simp only: left_inverse)
  also have "\<dots> = x \<bullet> x\<inverse> \<bullet> x" by (simp only: assoc)
  also have "\<dots> = \<one> \<bullet> x" by (simp only: right_inverse)
  also have "\<dots> = x" by (simp only: left_unit)
  finally show ?thesis .
qed

end

sublocale group \<subseteq> monoid
  by standard (simp_all add: left_unit right_unit)

interpretation monoid add 0
  by unfold_locales (simp_all add: add_assoc)

end
