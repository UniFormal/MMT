package info.kwarc.mmt.api.utils

/**
 * matches S1 in "beforeS1after"
 */
class StringMatcher(before: String, after: String) {
   private val baLength = before.length + after.length
   def apply(s: String) = before + s + after
   def unapply(s: String): Option[String] = {
      if (s.startsWith(before) && s.endsWith(after)) {
         Some(s.substring(before.length, s.length-baLength))
      } else
         None
   }
}

/**
 * matches S1, S2 in "beforeS1middleS2after"
 * 
 * first occurrence of 'middle' after 'before' is used if ambiguous
 */
class StringMatcher2(before: String, middle: String, after: String) {
   private val baLength = before.length + after.length
   private val mLength = middle.length
   def apply(s1: String, s2: String) = before + s1 + middle + s2 + after
   def unapply(s: String): Option[(String,String)] = {
      if (s.startsWith(before) && s.endsWith(after)) {
         val s12 = s.substring(before.length, s.length-baLength)
         val i = s12.indexOf(middle)
         if (i == -1) return None
         val s1 = s12.substring(0,i)
         val s2 = s12.substring(i+mLength)
         Some((s1,s2))
      } else
         None
   }
}

/**
 * matches S1, S2, S3 in "beforeS1middle1S2middle2S3after"
 *
 * first occurrence of 'middle1' after 'before' and 'middle2' after 'middle1' is used if ambiguous
 */
class StringMatcher3(before: String, middle1: String, middle2: String, after: String) {
   private val baLength = before.length + after.length
   private val m1Length = middle1.length
   private val m2Length = middle2.length
   def apply(s1: String, s2: String, s3: String) = before + s1 + middle1 + s2 + middle2 + s3 + after
   def unapply(s: String): Option[(String,String,String)] = {
      if (s.startsWith(before) && s.endsWith(after)) {
         val s123 = s.substring(before.length, s.length-baLength)
         val i = s123.indexOf(middle1)
         if (i == -1) return None
         val s23 = s.substring(i + m1Length)
         val j = s23.indexOf(middle2)
         if (j == -1) return None
         val s1 = s123.substring(0,i)
         val s2 = s23.substring(0,j)
         val s3 = s23.substring(j+m2Length)
         Some((s1,s2,s3))
      } else
         None
   }
}