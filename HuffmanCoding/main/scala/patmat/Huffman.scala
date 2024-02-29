package patmat

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface:

  // Part 1: Basics
  def weight(tree: CodeTree): Int = {
    tree match {
      case Fork(l, r, c, w) => weight(l) + weight(r)
      case Leaf(c, w) => w
    }
  }

  def chars(tree: CodeTree): List[Char] = {
    tree match {
      case Fork(l, r, c, w) => c
      case Leaf(c, w) => List(c)
    }
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    // Helper function to put the chars into a frequency map
    @tailrec
    def recurTimes(remChars: List[Char],
                   acc: mutable.HashMap[Char, Int]): mutable.HashMap[Char, Int] = {
      remChars match {
        // We have a non null list
        case x :: xs => {
          // if it's already in the map add 1 to its count
          if (acc.contains(x)) {
            acc(x) += 1
          } else {
            // otherwise add it to the map with an initial count of 1
            acc += x -> 1
          }
          recurTimes(xs, acc)
        }
        // we have no chars left so return the hash map
        case Nil =>  acc
      }
    }
    // build the hash map and then convert to a list
    recurTimes(chars, new mutable.HashMap[Char, Int]()).toList
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    // Helper function to build the ordered leaf list one frequency at a time
    @tailrec
    def buildLeafList(remFreqs: List[(Char, Int)], accLeafs: List[Leaf]): List[Leaf] = {
      remFreqs match {
        // if no frequencies left, return the accumulator
        case Nil => accLeafs
        // otherwise we have an element
        case x :: xs  => {
          // get the char and weight, put them in a Leaf at the head and recurse
          val leafChar = x._1
          val leafWeight = x._2
          buildLeafList(xs, Leaf(leafChar, leafWeight) :: accLeafs)
        }
      }
    }
    // start with an empty accumulator and the frequencies sorted by descending weight
    buildLeafList(freqs.sortBy((x, y) => -y), Nil)
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = {
    trees match {
      case x :: Nil => true
      case _ => false
    }
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    // A helper to insert tree in sorted order into restTrees
    def insert(tree: CodeTree, restTrees: List[CodeTree]): List[CodeTree] = {
      restTrees match {
        // If there are no other trees, then it is a 1 element list
        case Nil => tree :: Nil
        case x :: xs => {
          // if the weight is less than the weight of the head put it at the front
          if (weight(tree) <= weight(x)) {
            tree :: restTrees
          } else {
            // otherwise take the head followed by recursive call on the rest
            x :: insert(tree, xs)
          }
        }
      }
    }

    trees match {
      case x :: y :: ys => {
        // there are at least two elements in the tree, so combine them and insert in sorted order
        val parent = makeCodeTree(x, y)
        insert(parent, ys)
      }
      // One or less elements in the tree, so no work to do.
      case _ => trees
    }
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (done(trees)) {
      trees
    } else {
      until(done, merge)(merge(trees))
    }
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }


  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    // Helper function to decode to traverse one bit at a time
    def decodeRecur(subTree: CodeTree, remBits: List[Bit]): List[Char] = {
      remBits match {
        // if there are no more bits and we're at a leaf then the leaf is the last character.
        case Nil => {
          subTree match {
            case Leaf(c, w) => List(c)
            case _ => Nil
          }
        }
        case x :: xs => {
          subTree match {
            // if we're at a leaf decode to that character and recurse from the start
            case Leaf(c, w) => c :: decodeRecur(tree, remBits)
            // otherwise if the next bit is 0 go left, else go right
            case Fork(l, r, c, w) => {
              if (x == 0) {
                decodeRecur(l, xs)
              } else {
                decodeRecur(r, xs)
              }
            }
          }
        }
      }
    }

    tree match {
      // If the tree is a leaf no decoding can be done
      case Leaf(c, w) => Nil
      // otherwise decode recursively
      case _ => decodeRecur(tree, bits)
    }
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * Helper function to identify whether an element is in a list.
   */
  def inList(c: Char, charList: List[Char]): Boolean = {
    charList match {
      // If we reach the end of the list we didn't find it
      case Nil => false
      // otherwise if c is the head of the list return true, otherwise, recurse on tail
      case x :: xs => if (x == c) true else inList(c, xs)
    }
  }

  def encodeChar(currChar: Char, subTree: CodeTree): List[Bit] = {
    subTree match {
      // if its a leaf then because this is a helper (we only pass from encode if currChar is in the tree and we
      // always maintain the variant that currChar is in subTree) we assume we have reached the currChar leaf
      // and we end the list
      case Leaf(c, w) => Nil
      case Fork(l, r, c, w) => {
        // otherwise it's a fork and pick the subtree that has a char, adding 0 or 1 if we go left or right,
        // respectively.
        if (inList(currChar, chars(l))) {
          0 :: encodeChar(currChar, l)
        } else {
          1 :: encodeChar(currChar, r)
        }
      }
    }
  }

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    text match {
      case Nil => Nil
      case x :: xs => {
        if (inList(x, chars(tree))) {
          // the tree has the list of all chars. So if we have x in that
          // list encode x with the tree and then encode the rest.
          encodeChar(x, tree) ::: encode(tree)(xs)
        } else {
          // otherwise x is not in the list so just skip and encode the rest.
          encode(tree)(xs)
        }
      }
    }
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table match {
      // We have reached the end of the code table and didn't find the char
      case Nil => Nil
      case tableEntry :: restOfTable => {
        val entryChar = tableEntry._1
        val entryBits = tableEntry._2
        if (char == entryChar) {
          entryBits
        } else {
          codeBits(restOfTable)(char)
        }
      }
    }
  }

  def bitsToString(bits: List[Bit]): String = {
    def bitsToStringHelper(remBits: List[Bit]): String = {
      remBits match {
        case Nil => ""
        case x :: xs => x.toString + ", " + bitsToStringHelper(xs)
      }
    }

    "List(" + bitsToStringHelper(bits) + ")"
  }

  def codeTableToString(codeTable: CodeTable): String = {
    def codeTableToStringHelper(remCodeTable: CodeTable): String = {
      remCodeTable match {
        case Nil => ""
        case x :: xs => {
          val c = x._1
          val b = x._2
          "(" + c + ", " + bitsToString(b) + "), " + codeTableToStringHelper(xs)
        }
      }
    }

    "List(" + codeTableToStringHelper(codeTable) + ")"
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    def convertRecur(accBits: List[Bit], subtree: CodeTree): CodeTable = {
      subtree match {
        case Leaf(c, w) => List((c, accBits.reverse))
        case Fork(l, r, c, w) => mergeCodeTables(convertRecur(0 :: accBits, l), convertRecur(1 :: accBits, r))
      }
    }

    convertRecur(Nil, tree)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    // Note we will assume a comes from the left subtree and b comes from the right subtree
    // The characters in the left subtree are more frequent so they should be ordered first for
    // faster access
    a ::: b
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)

    @tailrec
    def quickEncodeHelper(remText: List[Char], accBits: List[Bit]): List[Bit] = {
      remText match {
        case Nil => accBits
        case x :: xs => quickEncodeHelper(xs, accBits ::: codeBits(codeTable)(x))
      }
    }

    quickEncodeHelper(text, Nil)
  }

object Huffman extends Huffman
