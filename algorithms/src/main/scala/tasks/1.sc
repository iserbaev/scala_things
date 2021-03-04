import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

/**
 *  Посчитать все последовательности одинаковых символов
 *  Ответ выдать в виде Seq[(Char, Int)] (символ и число последовательных повторений)
 */

val in = "Sstriings"

in.tail.foldLeft(((in.head,1),Seq.empty[(Char,Int)])){ case (((prev,count),acc),char) =>
  if (char == prev) {
    ((prev,count + 1),acc)
  } else {
    if (count > 1) {
      ((char,1),acc.+:((prev,count)))
    } else {
      ((char,1),acc)
    }
  }
}._2

/**
Дана строка s. Посчитать количество повторений каждой буквы, вывести в виде
List[(Char,Int)]

  val s: String = "aaabbcccdeeefgha"
   List(
   'a' -> 4,
   'b' -> 2,
   'c' -> 3,
   ...
    )
   - решить через groupBy
   - решить через foldLeft
  */

val s: String = "aaabbcccdeeefgha"

s.groupBy(identity).mapValues(_.length)

/**
Посчитать количество последовательностей  из отрицательных чисел не менее 3
например -2,-3,-4 это одна последовательность = каунтер 1
-2, -2, -1 - это вторая последовательность = каунтер 2

val list = List(1, -2, -3, -4, -1, 3, 1, -2, -2, -1, 0, -1, -1, -2)    ответ 3
val list2 = List(1, -2, -3, -4, -1)                                    ответ 1
val list3 = List(1, -2, -3, -4, -1, 3, 1, -2, -2, -1)                  ответ 2

 */

def negSeqCount(l: List[Int]): Int = {
  val (last,acc) = l.foldLeft((0,0)){ case ((counter, acc),el) =>
    if (el >= 0) {
      if (counter >= 3) {
        (0, acc + 1)
      } else {
        (0,acc)
      }
    } else {
      (counter + 1, acc)
    }
  }

  if (last >=3) acc + 1 else acc
}

negSeqCount(List(1, -2, -3, -4, -1, 3, 1, -2, -2, -1, 0, -1, -1, -2))
negSeqCount(List(1, -2, -3, -4, -1))
negSeqCount(List(1, -2, -3, -4, -1, 3, 1, -2, -2, -1))

/**
 Дана последовательность val in = Seq(1, 2, 3, -1, -2, 1, -1, -2, -3)
 нужно посчитать количество положительных и отрицательных последовательностей
 которых больше 3

 ответ 2
*/

def seqCount(l: List[Int]): Int = l match {
  case Nil => 0
  case ::(head, tl) =>
    val (_,counter,acc) = tl.foldLeft((head,1,0)){ case ((prev,counter,acc),el) =>
      if (prev * el > 0) {
        (el,counter + 1,acc)
      } else {
        (el,1, if (counter >= 3) acc + 1 else acc)
      }
    }

    if (counter >= 3) acc + 1 else acc
}

seqCount(Seq(1, 2, 3, -1, -2, 1, -1, -2, -3).toList)

/**
 * Transformation Chain
 * Дан набор возможных трансформаций: type Transformation[T] = T => Option[T]
 * Написать функцию преобразования последовательности трансформаций в возможную трансформацию.
 * Новая трансформация это результат работы всей цепочки трансформаций, которые не вернули None.
 * Если все вернули None, то общий результат None.
 */

type Transformation[T] = T => Option[T]
def sequence[T](ff: Seq[Transformation[T]]): Transformation[T] = (t:T) => {
  val filtered = ff.map(Function.unlift[T,T]).filter(_.isDefinedAt(t))
  if (filtered.isEmpty) None else {
    Some(filtered.foldLeft(t){ case (tt,pf) =>
      pf.applyOrElse[T,T](tt, _ => tt)
    })
  }
}
def sequence2[T](ff: Seq[Transformation[T]]): Transformation[T] = (t:T) => {
  val (transformed, count) = ff.foldLeft((t,0)){ case ((tt,trCount),f) =>
    f(tt) match {
      case Some(value) => (value,trCount + 1)
      case None =>(tt,trCount)
    }
  }

  if (count > 0) Some(transformed) else None
}

/*    На вход Seq[Future[String]]
  *     Получить Future[(Seq[String], Seq[Throwable]) - результат агрегации выполненых Future и исключений
  */


val tasks = Seq(
  Future.successful {
    Thread.sleep(1000)
    "red"
  },
  Future.failed(new RuntimeException("exception1")),
  Future.successful("blue"),
  Future.failed(new RuntimeException("exception2"))
)

val f = for {
  fseq <- Future.sequence(tasks.map(_.transform(Success(_))))
} yield {
  fseq.foldLeft((Seq.empty[String],Seq.empty[Throwable])){ case ((ss,thrs),tr) =>
    tr match {
      case Failure(exception) =>
        (ss,thrs.:+(exception))
      case Success(value) =>
        (ss.:+(value),thrs)
    }
  }
}

Await.result(f, Duration.Inf)

//  / Дан список произвольных элементов, пусть это будет List[Int]
//    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ..., 100)
//         -------  -------  -------  ---------  ...
//            f1       f2       f3        f4      ...
//
//  Задача: написать функцию def batchedSum(xs: List[Int], batchSize: Int):Future[Seq[Int]]
//  которая возвращает Future с последовательностью, содержащую сумму batches, на которые данный список разбивается
//  Причем, каждый batch должен вычисляться в отдельной Future (см. f1, f2, f3, ...)
//  Каждая Future (f1, f2, ...), ответственная за вычисление своего пакета, должна выполняться строго одна за другой.
//  Тоесть в следующей последовательности:
//    f1 -> f2 -> ...> fn
//
//  Тоесть, сначала вычисляется f1, затем - f2 и т.д. Другими словами, они не должны вычисляться параллельно.

def batchedSum(xs: List[Int], batchSize: Int):Future[Seq[Int]] = {
  val futures = for {
    batch <- xs.grouped(batchSize)
  } yield {
    Future{
      batch.sum
    }
  }

  Future.sequence(futures.toSeq)
}

val list = (1 to 100).toList
val fSum = batchedSum(list,3)
Await.result(fSum,3.seconds).toList

/**
 * Написать функцию `batchedFutures`, которая применит заданную функцию `f` к каждому элементу коллекции.
 * При этом, необходимо вычислить все получившиеся футуры (Futures) не все сразу параллельно, а пакетами с заданным размером `batchSize`.
 * Функция `batchedFutures` должна возвратить футуру со списком результатов применения ф-ции `f` к каждому из элементов.
 *
 * Пример:
 * {{{
 *     val xs = List(1,  2,  3,  4,  5,  6,  7,  8,  9,  ..., 100)
 *
 *                   |   |   |   |   |   |   |   |   |               //#1: применение ф-ции `f` к каждому элементу списка
 *                   v   v   v   v   v   v   v   v   v   ...
 *                   f1  f2  f3  f4  f5  f6  f7  f8  f9              //#2: асинхронные результаты применения ф-ции `f` к каждому элементу списка
 *                   _________  _________  __________
 *                     batch1      batch2      batch3                //#3: разбиение футур на пакеты заданного размера `batchSize=3`
 *
 *     batchedFutures(xs, batchSize = 3, (e:Int) => Future(-e))      //#4: передаем ф-цию, которая инвертирует знак элемента
 *      |
 *      |
 *      +---> должен вернуть Future[List[-1, -2, -3, ..., -100]]     //#5: результирующий список отрицательных элементов, вычисленный пакетами
 *
 * }}}
 *
 * Например, в примере выше, размер `batchSize=3`. Это значит, что футуры `f1`, `f2`, `f3`, ..., `fn` будут вычисляться пакетами:
 * 1. сначала параллельно вычислятся футуры первого пакета: `f1, f2, f3`
 * 2. затем параллельно вычислятся футуры второго пакета: `f4, f5, f6`
 * 3. и т.д., пока не вычислится последний пакет футур
 *
 * Необходимо проиллюстрировать, что футуры действительно выполняются параллельно в пределах пакета.
 * Продемонстрировать работу программы для разных batchSize: 1, 2, 4, 8, 10, 20
 */

def batchedFutures[E](xs: List[E], batchSize: Int, f: E => Future[E]): Future[List[E]] = {
  Future.sequence(for {
    batch <- xs.map(e => () => f(e)).grouped(batchSize)
  } yield {
    Future.traverse(batch)(_.apply())
  }).map(_.flatten.toList)
}

val fBatch = batchedFutures((1 to 100).toList, batchSize = 3, (e:Int) => Future(-e))
Await.result(fBatch,3.second)
