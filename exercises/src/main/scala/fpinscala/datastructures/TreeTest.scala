import org.scalatest._
import org.scalatest.Assertions._
import fpinscala.datastructures._

class TreeTest extends FlatSpec {

  "Function size" should "return the size of a given leaf" in {
    val tree = Leaf("Hello world")
    assert(1 == Tree.size(tree))
  }

  it should "return the size of a tree with two leafs" in {
    val tree = Branch(Leaf(2),Leaf(3))
    assert(3 == Tree.size(tree))
  }

  it should "return the size of a tree with two branches" in {
    val branch1 = Branch(Leaf(2),Leaf(2))
    val branch2 = Branch(Leaf(2),Leaf(2))
    val tree = Branch(branch1,branch2)
    assert(7 == Tree.size(tree))
  }

  "Function maximum" should "return the maximum integer element in a tree" in {
    val tree = Branch(Leaf(1), Leaf(2))
    assert(2 == Tree.maximum(tree))
  }

  "Function depth" should "return the depth of a tree" in {
    val tree = Leaf(2)
    assert(1 == Tree.depth(tree))
  }

  it should "return the depth of a two story tree" in {
    val tree = Branch(Leaf(2),Leaf(2))
    assert(2 == Tree.depth(tree))
  }

  it should "return the depth of an unbalanced tree" in {
    val tree = Branch(Branch(Leaf(2),Leaf(2)),Leaf(2))
    assert(3 == Tree.depth(tree))
  }

  "Function map" should "correctly map integers to strings" in {
    val tree = Branch(Leaf(1),Leaf(2))
    val result = Tree.map(tree)(elem => elem.toString())
    val expected = Branch(Leaf("1"),Leaf("2"))
    assert(expected == result)
  }

  "Function maximumFold" should "return the maximum integer element in a tree" in {
    val tree = Branch(Leaf(1), Leaf(2))
    assert(2 == Tree.maximumFold(tree))
  }
  "Function sizeFold" should "return the size of a given leaf" in {
    val tree = Leaf("Hello world")
    assert(1 == Tree.sizeFold(tree))
  }
  it should "return the size of a tree with two leafs" in {
    val tree = Branch(Leaf(2),Leaf(3))
    assert(3 == Tree.sizeFold(tree))
  }
  it should "return the size of a tree with two branches" in {
    val branch1 = Branch(Leaf(2),Leaf(2))
    val branch2 = Branch(Leaf(2),Leaf(2))
    val tree = Branch(branch1,branch2)
    assert(7 == Tree.sizeFold(tree))
  }
  "Function depthFold" should "return the depth of a tree" in {
    val tree = Leaf(2)
    assert(1 == Tree.depthFold(tree))
  }

  it should "return the depth of a two story tree" in {
    val tree = Branch(Leaf(2),Leaf(2))
    assert(2 == Tree.depthFold(tree))
  }
  it should "return the depth of an unbalanced tree" in {
    val tree = Branch(Branch(Leaf(2),Leaf(2)),Leaf(2))
    assert(3 == Tree.depthFold(tree))
  }
  "Function mapFold" should "correctly map integers to strings" in {
    val tree = Branch(Leaf(1),Leaf(2))
    val result = Tree.mapFold(tree)(elem => elem.toString())
    val expected = Branch(Leaf("1"),Leaf("2"))
    assert(expected == result)
  }
}
