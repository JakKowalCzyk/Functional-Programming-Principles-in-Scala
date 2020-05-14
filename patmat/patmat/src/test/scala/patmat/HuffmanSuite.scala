package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
      assertEquals(9, weight(t2))
      assertEquals(2, weight(Leaf('a', 2)))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
      assertEquals(List('a'), chars(Leaf('a', 2)))
    }

  @Test def `times equals`: Unit =
    assertEquals(List(('a', 2), ('b', 2), ('c', 1)), times(List('a', 'b', 'c', 'a', 'b')))

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))

  @Test def `singleton equals`: Unit =
    new TestTrees {
      assertEquals(true, singleton(List(Leaf('e',1))))
      assertEquals(false, singleton(List(Leaf('e',1), Leaf('t',2), Leaf('x',3))))
    }

  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
    assertEquals(List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3) ,Leaf('x',4), List('e', 't', 'x'), 7) ), combine(combine(leaflist)))
    val twolist = List(Leaf('e', 1), Leaf('t', 2))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)), combine(twolist))
    assertEquals(List(Leaf('e', 1)), combine(List(Leaf('e', 1))))
    assertEquals(List(), combine(List()))
  }

  @Test def `until equals`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3) ,Leaf('x',4), List('e', 't', 'x'), 7) ), until(singleton, combine)(leaflist))
    val twolist = List(Leaf('e', 1), Leaf('t', 2))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)), until(singleton, combine)(twolist))
    assertEquals(List(Leaf('e', 1)), until(singleton, combine)(List(Leaf('e', 1))))
  }

  @Test def `createCodeTree equals` : Unit = {
    new TestTrees {
      assertEquals(t1, createCodeTree(List('b', 'a', 'a', 'b', 'b')))
      assertEquals(Fork(Leaf('d',4),Fork(Leaf('a',2),Leaf('b',3),List('a', 'b'),5),List('d', 'a', 'b'),9), createCodeTree(List('b','d', 'a','d', 'a','d', 'b', 'b', 'd')))
    }
  }

  @Test def `decode equals`: Unit = {
    val tree =
      Fork(Leaf('a', 8), Fork(Leaf('b', 3), Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2), List('b', 'c', 'd'), 5), List('a', 'b', 'c', 'd'), 13)
    assertEquals("d".toList, decode(tree, List(1, 1, 1)))
    assertEquals("bacd".toList, decode(tree, List(1, 0, 0,  1, 1,0,1,1,1)))
    println(decodedSecret)
  }


  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      val tree =
        Fork(Leaf('a', 8), Fork(Leaf('b', 3), Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2), List('b', 'c', 'd'), 5), List('a', 'b', 'c', 'd'), 13)
      assertEquals(List(0), encode(tree)(List('a')))
      assertEquals(List(0,1,0), encode(tree)("ab".toList))
      assertEquals(List(1, 0, 0,  1, 1,0,1,1,1), encode(tree)("bacd".toList))
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `codeBits equals` : Unit ={
    val codeTable1 = ('b', List(1,1,1))
    val codeTable2 = ('a', List(0,1))
    val codeTable3 = ('c', List(0,0,0,1))
    val list = List(codeTable1, codeTable2, codeTable3)
    assertEquals(List(0,1), codeBits(list)('a'))
  }

  @Test def `mergeCodeTables equals` : Unit = {
    val table = mergeCodeTables(List((' ', List(0))), List(('b', List(0, 1)), ('a', List(1, 0))))
    assertEquals(List(('b', List(0,0,1)), ('a', List(0,1,0))), table)
  }

  @Test def `convert equals` : Unit = {
    new TestTrees {
      assertEquals(List(('a', List(0)), ('b', List(1))), convert(t1))
      assertEquals(List(('a', List(0, 0)), ('b', List(0,1)), ('d', List(1))), convert(t2))
    }
  }

  @Test def `decode and quickEncode`: Unit =
    new TestTrees {
      val tree =
        Fork(Leaf('a', 8), Fork(Leaf('b', 3), Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2), List('b', 'c', 'd'), 5), List('a', 'b', 'c', 'd'), 13)
      assertEquals(List(0), quickEncode(tree)(List('a')))
      assertEquals(List(0,1,0), quickEncode(tree)("ab".toList))
      assertEquals(List(1, 0, 0,  1, 1,0,1,1,1), quickEncode(tree)("bacd".toList))
      assertEquals("ab".toList, decode(t1, quickEncode(t1)("ab".toList)))
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
