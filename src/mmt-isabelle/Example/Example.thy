chapter \<open>Example proof document in Isabelle\<close>

theory Example
  imports Main
begin


section \<open>Cantor's Theorem\<close>

subsection \<open>Mathematical statement and proof\<close>

text \<open>
  Cantor's Theorem states that there is no surjection from
  a set to its powerset.  The proof works by diagonalization.  E.g.\ see
  \<^item> \<^url>\<open>http://mathworld.wolfram.com/CantorDiagonalMethod.html\<close>
  \<^item> \<^url>\<open>https://en.wikipedia.org/wiki/Cantor's_diagonal_argument\<close>
\<close>

theorem Cantor: "\<nexists>f :: 'a \<Rightarrow> 'a set. \<forall>A. \<exists>x. A = f x"
proof
  assume "\<exists>f :: 'a \<Rightarrow> 'a set. \<forall>A. \<exists>x. A = f x"
  then obtain f :: "'a \<Rightarrow> 'a set" where *: "\<forall>A. \<exists>x. A = f x" ..
  let ?D = "{x. x \<notin> f x}"
  from * obtain a where "?D = f a" by blast
  moreover have "a \<in> ?D \<longleftrightarrow> a \<notin> f a" by blast
  ultimately show False by blast
qed


subsection \<open>Automated proofs\<close>

text \<open>
  These automated proofs are much shorter, but lack information why and how it
  works.
\<close>

theorem "\<nexists>f :: 'a \<Rightarrow> 'a set. \<forall>A. \<exists>x. f x = A"
  by best

theorem "\<nexists>f :: 'a \<Rightarrow> 'a set. \<forall>A. \<exists>x. f x = A"
  by force


subsection \<open>Elementary version in higher-order predicate logic\<close>

text \<open>
  The subsequent formulation bypasses set notation of HOL; it uses elementary
  \<open>\<lambda>\<close>-calculus and predicate logic, with standard introduction and elimination
  rules. This also shows that the proof does not require classical reasoning.
\<close>

lemma iff_contradiction:
  assumes *: "\<not> A \<longleftrightarrow> A"
  shows False
proof (rule notE)
  show "\<not> A"
  proof
    assume A
    with * have "\<not> A" ..
    from this and \<open>A\<close> show False ..
  qed
  with * show A ..
qed

theorem Cantor': "\<nexists>f :: 'a \<Rightarrow> 'a \<Rightarrow> bool. \<forall>A. \<exists>x. A = f x"
proof
  assume "\<exists>f :: 'a \<Rightarrow> 'a \<Rightarrow> bool. \<forall>A. \<exists>x. A = f x"
  then obtain f :: "'a \<Rightarrow> 'a \<Rightarrow> bool" where *: "\<forall>A. \<exists>x. A = f x" ..
  let ?D = "\<lambda>x. \<not> f x x"
  from * have "\<exists>x. ?D = f x" ..
  then obtain a where "?D = f a" ..
  then have "?D a \<longleftrightarrow> f a a" by (rule arg_cong)
  then have "\<not> f a a \<longleftrightarrow> f a a" .
  then show False by (rule iff_contradiction)
qed


section \<open>Algebraic structures\<close>

subsection \<open>Groups\<close>

class group = times + one + inverse +
  assumes group_assoc: "(x * y) * z = x * (y * z)"
    and group_left_one: "1 * x = x"
    and group_left_inverse: "inverse x * x = 1"

lemma (in group) group_right_inverse: "x * inverse x = 1"
proof -
  have "x * inverse x = 1 * (x * inverse x)"
    by (simp only: group_left_one)
  also have "\<dots> = 1 * x * inverse x"
    by (simp only: group_assoc)
  also have "\<dots> = inverse (inverse x) * inverse x * x * inverse x"
    by (simp only: group_left_inverse)
  also have "\<dots> = inverse (inverse x) * (inverse x * x) * inverse x"
    by (simp only: group_assoc)
  also have "\<dots> = inverse (inverse x) * 1 * inverse x"
    by (simp only: group_left_inverse)
  also have "\<dots> = inverse (inverse x) * (1 * inverse x)"
    by (simp only: group_assoc)
  also have "\<dots> = inverse (inverse x) * inverse x"
    by (simp only: group_left_one)
  also have "\<dots> = 1"
    by (simp only: group_left_inverse)
  finally show ?thesis .
qed

lemma (in group) group_right_one: "x * 1 = x"
proof -
  have "x * 1 = x * (inverse x * x)"
    by (simp only: group_left_inverse)
  also have "\<dots> = x * inverse x * x"
    by (simp only: group_assoc)
  also have "\<dots> = 1 * x"
    by (simp only: group_right_inverse)
  also have "\<dots> = x"
    by (simp only: group_left_one)
  finally show ?thesis .
qed


subsection \<open>Groups as monoids\<close>

class monoid = times + one +
  assumes monoid_assoc: "(x * y) * z = x * (y * z)"
    and monoid_left_one: "1 * x = x"
    and monoid_right_one: "x * 1 = x"

instance group \<subseteq> monoid
  by intro_classes
    (rule group_assoc,
      rule group_left_one,
      rule group_right_one)


subsection \<open>More theorems of group theory\<close>

text \<open>
  The one element is already uniquely determined by preserving an \<^emph>\<open>arbitrary\<close>
  group element.
\<close>

lemma (in group) group_one_equality:
  assumes eq: "e * x = x"
  shows "1 = e"
proof -
  have "1 = x * inverse x"
    by (simp only: group_right_inverse)
  also have "\<dots> = (e * x) * inverse x"
    by (simp only: eq)
  also have "\<dots> = e * (x * inverse x)"
    by (simp only: group_assoc)
  also have "\<dots> = e * 1"
    by (simp only: group_right_inverse)
  also have "\<dots> = e"
    by (simp only: group_right_one)
  finally show ?thesis .
qed

text \<open>
  Likewise, the inverse is already determined by the cancel property.
\<close>

lemma (in group) group_inverse_equality:
  assumes eq: "x' * x = 1"
  shows "inverse x = x'"
proof -
  have "inverse x = 1 * inverse x"
    by (simp only: group_left_one)
  also have "\<dots> = (x' * x) * inverse x"
    by (simp only: eq)
  also have "\<dots> = x' * (x * inverse x)"
    by (simp only: group_assoc)
  also have "\<dots> = x' * 1"
    by (simp only: group_right_inverse)
  also have "\<dots> = x'"
    by (simp only: group_right_one)
  finally show ?thesis .
qed

text \<open>
  The inverse operation has some further characteristic properties.
\<close>

lemma (in group) group_inverse_times: "inverse (x * y) = inverse y * inverse x"
proof (rule group_inverse_equality)
  show "(inverse y * inverse x) * (x * y) = 1"
  proof -
    have "(inverse y * inverse x) * (x * y) = (inverse y * (inverse x * x)) * y"
      by (simp only: group_assoc)
    also have "\<dots> = (inverse y * 1) * y"
      by (simp only: group_left_inverse)
    also have "\<dots> = inverse y * y"
      by (simp only: group_right_one)
    also have "\<dots> = 1"
      by (simp only: group_left_inverse)
    finally show ?thesis .
  qed
qed

lemma (in group) inverse_inverse: "inverse (inverse x) = x"
proof (rule group_inverse_equality)
  show "x * inverse x = one"
    by (simp only: group_right_inverse)
qed

lemma (in group) inverse_inject:
  assumes eq: "inverse x = inverse y"
  shows "x = y"
proof -
  have "x = x * 1"
    by (simp only: group_right_one)
  also have "\<dots> = x * (inverse y * y)"
    by (simp only: group_left_inverse)
  also have "\<dots> = x * (inverse x * y)"
    by (simp only: eq)
  also have "\<dots> = (x * inverse x) * y"
    by (simp only: group_assoc)
  also have "\<dots> = 1 * y"
    by (simp only: group_right_inverse)
  also have "\<dots> = y"
    by (simp only: group_left_one)
  finally show ?thesis .
qed


section \<open>Peano's axioms for Natural Numbers\<close>

locale peano =
  fixes zero :: 'n
  fixes succ :: "'n \<Rightarrow> 'n"
  assumes succ_neq_zero [simp]: "succ m \<noteq> zero"
  assumes succ_inject [simp]: "succ m = succ n \<longleftrightarrow> m = n"
  assumes induct [case_names zero succ, induct type: 'n]:
    "P zero \<Longrightarrow> (\<And>n. P n \<Longrightarrow> P (succ n)) \<Longrightarrow> P n"
begin

lemma zero_neq_succ [simp]: "zero \<noteq> succ m"
  by (rule succ_neq_zero [symmetric])


text \<open>Primitive recursion as a (functional) relation -- polymorphic!\<close>

inductive Rec :: "'a \<Rightarrow> ('n \<Rightarrow> 'a \<Rightarrow> 'a) \<Rightarrow> 'n \<Rightarrow> 'a \<Rightarrow> bool"
  for e :: 'a and r :: "'n \<Rightarrow> 'a \<Rightarrow> 'a"
where
  Rec_zero: "Rec e r zero e"
| Rec_succ: "Rec e r m n \<Longrightarrow> Rec e r (succ m) (r m n)"

lemma Rec_functional: "\<exists>!y::'a. Rec e r x y" for x :: 'n
proof -
  let ?R = "Rec e r"
  show ?thesis
  proof (induct x)
    case zero
    show "\<exists>!y. ?R zero y"
    proof
      show "?R zero e" ..
      show "y = e" if "?R zero y" for y
        using that by cases simp_all
    qed
  next
    case (succ m)
    from \<open>\<exists>!y. ?R m y\<close>
    obtain y where y: "?R m y" and yy': "\<And>y'. ?R m y' \<Longrightarrow> y = y'"
      by blast
    show "\<exists>!z. ?R (succ m) z"
    proof
      from y show "?R (succ m) (r m y)" ..
    next
      fix z
      assume "?R (succ m) z"
      then obtain u where "z = r m u" and "?R m u"
        by cases simp_all
      with yy' show "z = r m y"
        by (simp only:)
    qed
  qed
qed


text \<open>The recursion operator -- polymorphic!\<close>

definition rec :: "'a \<Rightarrow> ('n \<Rightarrow> 'a \<Rightarrow> 'a) \<Rightarrow> 'n \<Rightarrow> 'a"
  where "rec e r x = (THE y. Rec e r x y)"

lemma rec_eval:
  assumes Rec: "Rec e r x y"
  shows "rec e r x = y"
  unfolding rec_def
  using Rec_functional and Rec by (rule the1_equality)

lemma rec_zero [simp]: "rec e r zero = e"
proof (rule rec_eval)
  show "Rec e r zero e" ..
qed

lemma rec_succ [simp]: "rec e r (succ m) = r m (rec e r m)"
proof (rule rec_eval)
  let ?R = "Rec e r"
  have "?R m (rec e r m)"
    unfolding rec_def using Rec_functional by (rule theI')
  then show "?R (succ m) (r m (rec e r m))" ..
qed


text \<open>Example: addition (monomorphic)\<close>

definition add :: "'n \<Rightarrow> 'n \<Rightarrow> 'n"
  where "add m n = rec n (\<lambda>_ k. succ k) m"

lemma add_zero [simp]: "add zero n = n"
  and add_succ [simp]: "add (succ m) n = succ (add m n)"
  unfolding add_def by simp_all

lemma add_assoc: "add (add k m) n = add k (add m n)"
  by (induct k) simp_all

lemma add_zero_right: "add m zero = m"
  by (induct m) simp_all

lemma add_succ_right: "add m (succ n) = succ (add m n)"
  by (induct m) simp_all

lemma "add (succ (succ (succ zero))) (succ (succ zero)) =
    succ (succ (succ (succ (succ zero))))"
  by simp


text \<open>Example: replication (polymorphic)\<close>

definition repl :: "'n \<Rightarrow> 'a \<Rightarrow> 'a list"
  where "repl n x = rec [] (\<lambda>_ xs. x # xs) n"

lemma repl_zero [simp]: "repl zero x = []"
  and repl_succ [simp]: "repl (succ n) x = x # repl n x"
  unfolding repl_def by simp_all

lemma "repl (succ (succ (succ zero))) True = [True, True, True]"
  by simp

end


text \<open>Just see that our abstract specification makes sense \dots\<close>

interpretation peano 0 Suc
proof
  fix m n
  show "Suc m \<noteq> 0" by simp
  show "Suc m = Suc n \<longleftrightarrow> m = n" by simp
  show "P n"
    if zero: "P 0"
    and succ: "\<And>n. P n \<Longrightarrow> P (Suc n)"
    for P
  proof (induct n)
    case 0
    show ?case by (rule zero)
  next
    case Suc
    then show ?case by (rule succ)
  qed
qed


section \<open>Finite sequences\<close>

datatype 'a seq = Empty | Seq 'a "'a seq"

fun conc :: "'a seq \<Rightarrow> 'a seq \<Rightarrow> 'a seq"
where
  "conc Empty ys = ys"
| "conc (Seq x xs) ys = Seq x (conc xs ys)"

fun reverse :: "'a seq \<Rightarrow> 'a seq"
where
  "reverse Empty = Empty"
| "reverse (Seq x xs) = conc (reverse xs) (Seq x Empty)"

lemma conc_empty: "conc xs Empty = xs"
  by (induct xs) simp_all

lemma conc_assoc: "conc (conc xs ys) zs = conc xs (conc ys zs)"
  by (induct xs) simp_all

lemma reverse_conc: "reverse (conc xs ys) = conc (reverse ys) (reverse xs)"
  by (induct xs) (simp_all add: conc_empty conc_assoc)

lemma reverse_reverse: "reverse (reverse xs) = xs"
  by (induct xs) (simp_all add: reverse_conc)

end
