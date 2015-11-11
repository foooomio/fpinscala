sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](t: Tree[A]): Int = t match {
        case Branch(l, r) => size(l) + size(r) + 1
        case _ => 1
    }

    def maximum(t: Tree[Int]): Int = t match {
        case Branch(l, r) => maximum(l) max maximum(r)
        case Leaf(v) => v
    }

    def depth[A](t: Tree[A]): Int = t match {
        case Branch(l, r) => depth(l) max depth(r) + 1
        case _ => 0
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
    }

    def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
        case Leaf(v) => f(v)
    }

    def size2[A](t: Tree[A]): Int =
        fold(t)(v => 1)(_ + _ + 1)

    def maximum2(t: Tree[Int]): Int =
        fold(t)(v => v)(_ max _)

    def depth2[A](t: Tree[A]): Int =
        fold(t)(v => 0)(_ max _ + 1)

    def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
        fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
}

val tree = Branch(Branch(Leaf(3), Leaf(9)), Branch(Leaf(2), Leaf(5)))
println(tree);
println(Tree.size2(tree));
println(Tree.maximum2(tree));
println(Tree.depth2(tree));
println(Tree.map2(tree)(_ * 2));
