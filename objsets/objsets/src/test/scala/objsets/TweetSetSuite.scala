package objsets

import org.junit._
import org.junit.Assert.assertEquals

class TweetSetSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val setcd = set1.incl(c).incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def asSet(list: TweetList) : Set[Tweet] = {
    var res = Set[Tweet]()
    list.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  def size(list: TweetList): Int = asSet(list).size

  @Test def `filter: on empty set`: Unit =
    new TestSets {
      assertEquals(0, size(set1.filter(tw => tw.user == "a")))
      assertEquals(1, size(set4c.filter(tw => tw.user == "c")))
      assertEquals(4, size(set5.filter(tw => tw.text.contains("body"))))
    }

  @Test def `filter: a on set5`: Unit =
    new TestSets {
      assertEquals(1, size(set5.filter(tw => tw.user == "a")))
    }

  @Test def `filter: twenty on set5`: Unit =
    new TestSets {
      assertEquals(2, size(set5.filter(tw => tw.retweets == 20)))
    }

  @Test def `union: set4c and set4d`: Unit =
    new TestSets {
      assertEquals(4, size(set4c.union(set4d)))
    }

  @Test def `union: with empty set1`: Unit =
    new TestSets {
      assertEquals(4, size(set5.union(set1)))
    }

  @Test def `union: with empty set2`: Unit =
    new TestSets {
      assertEquals(4, size(set1.union(set5)))
    }

  @Test def `descending: set5`: Unit =
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(size(trends)==4)
      assert(trends.head.user == "a" || trends.head.user == "b")
      assert(trends.tail.head.user == "a" || trends.tail.head.user == "b")
      assert(trends.tail.tail.head.user == "d" )
      assert(trends.tail.tail.tail.head.user == "c" )
    }

  @Test def `mostRetweeted`: Unit =
    new TestSets {
      assertEquals("a", set2.mostRetweeted.user)
      assertEquals("a", set3.mostRetweeted.user)
      assertEquals("a", set4c.mostRetweeted.user)
      assertEquals("d", setcd.mostRetweeted.user)
    }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
