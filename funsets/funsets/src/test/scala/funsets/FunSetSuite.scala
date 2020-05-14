package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s12 = singletonSet(1)
    val smaller = union(s1, s2)
    val s = union(smaller, s3)
    val n = set(x => (x < -980) || (x > 0))
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val un = union(s1, s2)
      assert(contains(un, 1), "Union 1")
      assert(contains(un, 2), "Union 2")
      assert(!contains(un, 3), "Union 3")
    }
  }

  @Test def `intersect contains all elements of each set`: Unit = {
    new TestSets {
      val inter = intersect(s1, s2)
      assert(!contains(inter, 1), "Intersect 1")
      assert(!contains(inter, 2), "Intersect 2")
      assert(!contains(inter, 3), "Intersect 3")

      val inters = intersect(s1,s12)
      assert(contains(inters, 1), "Intersect 1")
      assert(!contains(inters, 2), "Intersect 2")
      assert(!contains(inters, 3), "Intersect 3")

    }
  }

  @Test def `diff contains all elements of each set`: Unit = {
    new TestSets {
      val diffed = diff(s, smaller)
      assert(!contains(diffed, 1), "Diff 1")
      assert(!contains(diffed, 2), "Diff 2")
      assert(contains(diffed, 3), "Diff 3")
    }
  }

  @Test def `filter contains all elements of each set`: Unit = {
    new TestSets {
      val fil1 = filter(s, x=> x==1)
      assert(contains(fil1, 1), "filter 1")
      assert(!contains(fil1, 2), "filter 2")

      val fil2 = filter(s, x => x > 1)
      assert(contains(fil2, 3), "filter 3")
      assert(contains(fil2, 2), "filter 4")
      assert(!contains(fil2, 1), "filter 5")
    }
  }


  @Test def `forall contains all elements of each set`: Unit = {
    new TestSets {
      assert(forall(s1, p => p==1), "forall1")
      assert(!forall(s1, p => p > 1), "forall2")
      assert(!forall(s1, p => p < 1), "forall3")

      assert(forall(s, p => p > 0), "forall4")
      assert(!forall(s, p => p > 1), "forall4")

      assert(!forall(n, p => p ==0 ))
      assert(!forall(n, p => p >0 ))
      assert(!forall(n, p => p < 0 ))
      assert(forall(n, p => p < -500 || p > -90  ))
    }
  }

  @Test def `exists contains at least one element in set`: Unit = {
    new TestSets {
      assert(exists(s1, p => p==1), "exists1")
      assert(!exists(s1, p => p > 1), "exists2")
      assert(exists(s1, p => p >= 1), "exists2")
      assert(!exists(s1, p => p < 1), "exists3")

      assert(exists(s, p => p > 0), "exists4")
      assert(exists(s, p => p > 2), "exists4")
      assert(exists(s, p => p >= 3), "exists4")
      assert(!exists(s, p => p < 1), "exists5")
      assert(exists(s, p => p==1), "exists6")

      assert(!exists(n, p => p == 0 ))
      assert(exists(n, p => p == 1 ))
      assert(exists(n, p => p >0 ))
      assert(exists(n, p => p < 0 ))
      assert(exists(n, p => p < -500 || p > -90  ))
      assert(!exists(n, p => p < -500 && p > -90  ))
      assert(!exists(n, p => p > -500 && p < -90  ))
    }
  }







  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
