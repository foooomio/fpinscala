//package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def tail[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) => xs
    }

    def setHead[A](l: List[A], a: A): List[A] = l match {
        case Nil => Cons(a, Nil)
        case Cons(x, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) if(n > 0) => drop(xs, n - 1)
        case _ => l
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) if(f(x)) => dropWhile(xs, f)
        case _ => l
    }

    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _)

    def length[A](as: List[A]): Int =
        foldRight(as, 0)((_,n) => n + 1)

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

    def sum3(ns: List[Int]) =
        foldLeft(ns, 0)(_ + _)

    def product3(ns: List[Double]) =
        foldLeft(ns, 1.0)(_ * _)

    def length2[A](as: List[A]): Int =
        foldLeft(as, 0)((n,_) => n + 1)

    def reverse[A](as: List[A]): List[A] =
        foldLeft(as, Nil:List[A])((l,a) => Cons(a, l))

    def append[A](as: List[A], a: A): List[A] =
        foldRight(as, Cons(a, Nil))((x,y) => Cons(x, y))

    def concat[A](as1: List[A], as2: List[A]): List[A] =
        foldLeft(as2, as1)(append)

    def increment(ns: List[Int]): List[Int] =
        foldRight(ns, Nil:List[Int])((n,l) => Cons(n+1, l))

    def toStrings(ns: List[Double]): List[String] =
        foldRight(ns, Nil:List[String])((n,l) => Cons(n.toString, l))

    def map[A,B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil:List[B])((a,l) => Cons(f(a), l))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        foldRight(as, Nil:List[A])((a,l) => if(f(a)) Cons(a, l) else l)

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        foldRight(as, Nil:List[B])((a,l) => concat(f(a), l))

    def filter2[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)(a => if(f(a)) Cons(a, Nil) else Nil)

    def sumWith(as1: List[Int], as2: List[Int]): List[Int] =
        (as1, as2) match {
            case (Cons(x1, xs1), Cons(x2, xs2)) =>
                Cons(x1 + x2, sumWith(xs1, xs2))
            case _ => Nil
        }

    def zipWith[A](as1: List[A], as2: List[A])(f: (A,A) => A): List[A] =
        (as1, as2) match {
            case (Cons(x1, xs1), Cons(x2, xs2)) =>
                Cons(f(x1, x2), zipWith(xs1, xs2)(f))
            case _ => Nil
        }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
        (sup, sub) match {
            case (_, Nil) => true
            case (Nil, _) => false
            case (Cons(x1, xs1), Cons(x2, xs2)) if(x1 == x2) =>
                hasSubsequence(xs1, xs2)
            case (Cons(x1, xs1), Cons(x2, xs2)) =>
                hasSubsequence(xs1, sub)
        }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
}
println(x)

val list = List(1,2,3,4,5)
println(list)
println(List.tail(list))
println(List.setHead(list, 10))
println(List.setHead(Nil, 1))
println(List.drop(list, 3))
println(List.dropWhile(list, (x: Int) => x < 4))
println(List.init(list))
println(List.foldRight(list, Nil:List[Int])(Cons(_,_)))
println(List.length(list))
println(List.sum3(list))
println(List.product3(List(1.0,2.0)))
println(List.length2(list))
println(List.reverse(list))
println(List.append(list, 6))
println(List.concat(list, List(6,7,8,9)))
println(List.increment(list))
println(List.toStrings(List(1.1,1.2,1.3)))
println(List.map(list)(_ * 2))
println(List.filter(list)(_ % 2 == 0))
println(List.flatMap(list)(i => List(i,i)))
println(List.filter2(list)(_ % 2 == 0))
println(List.sumWith(list, list))
println(List.zipWith(list, list)(_ * _))
println(List.hasSubsequence(list, List(1,2,3)))
println(List.hasSubsequence(list, List(7,8,9)))
