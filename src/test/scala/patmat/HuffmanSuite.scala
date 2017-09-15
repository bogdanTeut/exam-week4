package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = List(Leaf('c', 1), Leaf('d', 1), Leaf('e', 1), Leaf('f', 1), Leaf('g', 1), Leaf('h', 1), Leaf('b', 3), Leaf('a', 8))
    val a = Leaf('a', 8)
    val b = Leaf('b', 3)
    val c = Leaf('c', 1)
    val d = Leaf('d', 1)
    val e = Leaf('e', 1)
    val f = Leaf('f', 1)
    val g = Leaf('g', 1)
    val h = Leaf('h', 1)
    val cd = Fork(c, d, List('c', 'd'), 2)
    val ef = Fork(e, f, List('e', 'f'), 2)
    val gh = Fork(g, h, List('g', 'h'), 2)
    val cdef = Fork(cd, ef, List('c', 'd', 'e', 'f'), 4)
    val ghb = Fork(gh, b, List('g', 'h', 'b'), 5)
    val cdefghb = Fork(cdef, ghb, List('c', 'd', 'e', 'f', 'g', 'h', 'b'), 9)
    val abcdefgh = Fork(a, cdefghb, List('a', 'c', 'd', 'e', 'f', 'g', 'h', 'b'), 17)

    val codeTableABCDEFGH: CodeTable = List(
                                    ('a', List(0)),
                                    ('c', List(1,0,0,0)),
                                    ('d', List(1,0,0,1)),
                                    ('e', List(1,0,1,0)),
                                    ('f', List(1,0,1,1)),
                                    ('g', List(1,1,0,0)),
                                    ('h', List(1,1,0,1)),
                                    ('b', List(1,1,1))
                                  )

    val codeTableACDE: CodeTable = List(
      ('a', List(0)),
      ('c', List(1,0,0,0)),
      ('d', List(1,0,0,1)),
      ('e', List(1,0,1,0))
    )

    val codeTableFGHB: CodeTable = List(
      ('f', List(1,0,1,1)),
      ('g', List(1,1,0,0)),
      ('h', List(1,1,0,1)),
      ('b', List(1,1,1))
    )

	}

  test("weight of a leaf") {
    new TestTrees {
      assert(weight(Leaf('a',2)) === 2)
    }
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a complex tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("chars of a leaf") {
    new TestTrees {
      assert(chars(Leaf('a', 2)) === List('a'))
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("make code tree") {
    new TestTrees {
      assert(makeCodeTree(t1, Leaf('d',4)) === t2)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(List('a', 'b', 'a'))") {
    new TestTrees {
      assert(times(List('a', 'b', 'a')) === List(('b', 1), ('a', 2)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(List(t1)) === true)
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("combine one element list") {
    new TestTrees {
      assert(combine(List(t1)) === List(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)))
    }
  }

  test("combine few leafs list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("until") {
    new TestTrees {
      assert(until(singleton, combine)(List(Leaf('e', 2))) == List(Leaf('e', 2)))
      assert(until(singleton, combine)(t3) == List(abcdefgh))
    }
  }

  test("createCodeTree") {
    new TestTrees {
      assert(createCodeTree(List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'c', 'd', 'e', 'f', 'g', 'h')) === abcdefgh)
    }
  }

  test("decode") {
    new TestTrees {
      assert(decode(abcdefgh, List(1,0,0,1,1,0,1,0,1,0,0,0,1,0,0,0,0,1,0,1,1)) === "deccaf".toList)
    }
  }

  test("decode french code") {
    new TestTrees {
      assert(decodedSecret === "huffmanestcool".toList)
    }
  }

  test("encode") {
    new TestTrees {
      assert(encode(abcdefgh)("deccaf".toList) === List(1,0,0,1,1,0,1,0,1,0,0,0,1,0,0,0,0,1,0,1,1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    new TestTrees {
      assert(codeBits(codeTableABCDEFGH)('d') === List(1,0,0,1))
    }
  }

  test("merge") {
    new TestTrees {
      assert(mergeCodeTables(codeTableACDE, codeTableFGHB) === codeTableABCDEFGH)
    }
  }

  test("convert") {
    new TestTrees {
      assert(convert(abcdefgh) === codeTableABCDEFGH)
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
    }
  }

//  List((a,List(0)), (c,List(1, 0, 0, 0)), (d,List(1, 0, 0, 1)), (e,List(1, 0, 1, 0)), (f,List(1, 0, 1, 1)), (g,List(1, 1, 0, 0)), (h,List(1, 1, 0, 1)), (b,List(1, 1, 1))) did not equal
//  List((a,List(0)), (b,List(1, 1, 1)), (c,List(1, 0, 0, 0)), (d,List(1, 0, 0, 1)), (e,List(1, 0, 1, 0)), (f,List(1, 0, 1, 1)), (g,List(1, 1, 0, 0)), (h,List(1, 1, 0, 1)))


}
