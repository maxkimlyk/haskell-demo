--------------------------------------------------------
--    __ __         __       ____  ___                --
--   / // /__ ____ / /_____ / / / / _ \___ __ _  ___  --
--  / _  / _ `(_-</  '_/ -_) / / / // / -_)  ' \/ _ \ --
-- /_//_/\_,_/___/_/\_\\__/_/_/ /____/\__/_/_/_/\___/ --
--------------------------------------------------------

-- Это демо модуль на языке Haskell, в котором показаны основные особенности возможности языка.

-- Материал берется в основном отсюда:
-- http://wiki.nsunc.com/_export/html/haskell
-- и отсюда:
-- https://stepik.org/course/75/

-------------------------------------------------------------------------------
-- Полезные команды оболочки ghci
-------------------------------------------------------------------------------
{-

:q, :quit     - выйти
:l, :load     - загрузить программу
:r, :reload   - перезагрузить текущий модуль
:t, :type     - напечатать сигнатуру функции
:k, :kind     - напечатать вид типа
:i, :info     - напечатать сигнатуру функции и указать, в каком файле функция была определена
:?, :h, :help - help

let используется, чтобы определять новые функции:

> let {f :: Int -> Int; f n = n * 2}

> :{
| let f :: Int -> Int
|     f n = n * 2
> :}

-}

-------------------------------------------------------------------------------
-- Модули
-------------------------------------------------------------------------------
-- Это название модуля, определяемого в данном файле
module Demo where

-- Импортирование других модулей
import Data.Char                              -- полностью
import Data.List (find)                       -- только некоторые функции
import Control.Monad.State hiding (withState) -- кроме заданных
import qualified Data.Set                     -- для всех функций из этого модуля должны использоваться полные имена
import Data.Word as Word                      -- импорт с назначением псевдонима

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans

import Prelude hiding (
    id, const, flip, (.), ($!),
    length, (++), null, last, init, reverse, zip, zip3, unzip,
    take, drop, splitAt, (!!),
    filter, takeWhile, dropWhile, span, break,
    map, concat, concatMap, all, any, zipWith, zipWith3,
    Monoid, mempty, mappend
    )

-------------------------------------------------------------------------------
-- Определение функций
-------------------------------------------------------------------------------
-- У функции может быть явно задана сигнатура, но это не обязательно.
fact :: Integer -> Integer
-- Шаблонное определение функции:
fact 0 = 1
fact n = n * fact (n-1)

-- или через гарды (охранные выражения):
fact' 0 = 1
fact' x | x <= 1    = 1
        | otherwise = x * fact' (x-1)

sign 0 = 0
sign x | x > 0     = 1
       | otherwise = (-1)

-------------------------------------------------------------------------------
-- Ветвления
-------------------------------------------------------------------------------
nosign x = if x >= 0 then x else negate x

sign'' x = if x >= 0
           then (if x == 0 then 0 else 1)
           else -1

casing x y = case (x, y) of
            (_, 0) -> x                        -- _ подходит под любую переменную,
            (_, n) -> 1 + casing x (n-1)       -- подставленную на это место.

-------------------------------------------------------------------------------
-- Частичное применение функций, каррирование
-------------------------------------------------------------------------------
add x y = x + y

-- Частичное применение
inc = add 1
-- add :: a -> a -> a
-- inc :: a -> a

-- Каррирование.
notCarried = fst    -- принимает двухэлементный кортеж,
-- notCarried :: (a, a) - > a
carried = curry fst -- принимает уже 2 аргумента,
-- carried :: a -> a -> a

-- uncarry выполняет ровно обратное действие.

-------------------------------------------------------------------------------
-- Лямбды
-------------------------------------------------------------------------------
inc' = \x -> x + 1
add' = \x y -> x + y

-------------------------------------------------------------------------------
-- Префиксный и постфиксный стиль
-------------------------------------------------------------------------------
-- Функции можно вызывать как в префиксном так и в постфиксном стилях:
max1 = max 10 28
max2 = 10 `max` 28

-- И опраторы можно вызывать как в префиксном так и в постфиксном стилях:
sum1 = 10 + 28
sum2 = (+) 10 28

-- Единственное исколючение - унарным минус
minus10 = (-) 10   -- это частично применненный бинарный оператор вычитания
numMinus10 = (-10) -- а это число -10

-------------------------------------------------------------------------------
-- Операторы
-------------------------------------------------------------------------------
-- Чтобы определять операторы, можно использовать следующие символы:
-- ! # $ % & * + . / < = > ? @ \ ^ | - ~ :
-- Но нельзя начинать имя оператора с символа :

-- Орератор сумма квадратов
infixl 6 *+*
a *+* b = a ^ 2 + b ^ 2
-- можно было определить и префиксно:
(*+**) a b = a ^ 2 + b ^ 2

-- Для частичного применения операторов есть специальный синтаксис (сечения):
twoDivBy = (2 /)
divByTwo = (/ 2)

-------------------------------------------------------------------------------
-- Левая и правая ассициативность операторов
-------------------------------------------------------------------------------
-- Левая ассоциативность:
-- x - y - z = (x - y) - z
-- Правая ассоциативность:
-- x ^ y ^ z = x ^ (y ^ z)

-- Из стандартной библиотеки:
-- infixr 8 ^, `logBase`
-- infixl 7 *, /, `div`, `mod`
-- infixl 6 +, -
-- infix 4 ==, /=, >, >=, <, <=

-- Если приоритет и ассоциативность оператора не заданы, то haskell преполагает,
-- что он имеет левую ассоциативность (infixl) и его приориет равен 9

-------------------------------------------------------------------------------
-- Оператор $
-------------------------------------------------------------------------------
-- Этот оператор ничего не вычисляет:
infixr 0 &
f & x = f x
-- но, имея приоритет 0, он позволяет избежать избыточного применения скобок:
sinPi2  = sin (pi / 2)  -- = 1
sinPi2' = sin $ pi / 2  -- = 1

-------------------------------------------------------------------------------
-- Базовые типы в haskell
-------------------------------------------------------------------------------
-- Char
-- Bool
-- Int - целые ограниченного размера
-- Integer - целые произвольного размера
-- Float
-- Double

-- Кортежи
cort = (2, True)
cort' = (,) True 3
cort3 = (,,) 1 'a' True
emptyCort = ()

-- fst возвращает первый элемент двухэлементного кортежа
cort1 = fst cort
-- snd возвращает второй элемент двухэлементного кортежа
cort2 = snd cort

-- Списки
list1 = [1, 2, 3]
list2 = [True, False, False]
str1 = "hello" -- список типа Char

-- : - оператор добавления элемента в голову списка
str2 = 'W' : "elcome"  -- = "Welcome"

-- (++) - оператор конкатенации списков
str3 = "Wel" ++ "come" -- = "Welcome"

-------------------------------------------------------------------------------
-- error и undefined
-------------------------------------------------------------------------------
-- error "Message" - прерывает выполнение программы и выводит сообщение "Message".
-- undefined - прерывает выполнение программы и выводит стандартное сообщение об ошибке.

fact'' 0 = 1
fact'' n = if n < 0 then undefined else n * fact (n-1)

-------------------------------------------------------------------------------
-- Конструкции let-in и where
-------------------------------------------------------------------------------
squareRoots a b c =
    let
        x1 = (-b - sqrtD) / (2 * a)
        x2 = (-b + sqrtD) / (2 * a)
        d = b^2 - 4 * a * c
        sqrtD = sqrt (d)
    in (x1, x2)

squareRoots' a b c = (x1, x2) where
    x1 = (-b - sqrtD) / (2 * a)
    x2 = (-b + sqrtD) / (2 * a)
    d = b^2 - 4 * a * c
    sqrtD = sqrt (d)

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2
    where
        x1 = fst p1
        x2 = fst p2
        y1 = snd p1
        y2 = snd p2

-------------------------------------------------------------------------------
-- Использование вспомогательных функций (пример)
-------------------------------------------------------------------------------
fact''' n | n >= 0 = accumulate 1 n
          | otherwise = error "Argument of factorial can not be negative"
    where
        accumulate acc 0 = acc
        accumulate acc n = accumulate (acc * n) (n-1)

-------------------------------------------------------------------------------
-- Определения некоторых стандартных функций
-------------------------------------------------------------------------------
id x = x
const x y = x
flip f y x = f x y
f . g = \x -> f (g x)

-------------------------------------------------------------------------------
-- Классы типов
-------------------------------------------------------------------------------
-- Класс типов описывает некий интерфейс, которому должны удовлетворять все его представители.

-- Класс типов, для которых определен оператор не равно:
-- Запись (Eq a => MP a) по сути означает, что класс типов MP расширяет класс типов Eq.
class Eq a => MP a where
    (*+) :: a -> a -> a

    x *+ y = if x == y then x else y  -- можно определять методы по-умолчанию

-- Представители класса типа MP:
instance MP Bool where
    x *+ False = False
    _ *+ _ = True   -- символ _ означает "во всех остальных случаях"

instance (MP a, MP b) => MP (a,b) where
    p1 *+ p2 = (fst p1 *+ fst p2, snd p1 *+ snd p2)

-- Зацикленное перечисление (пример)
class (Eq a, Enum a, Bounded a) => SafeEnum a where
    ssucc :: a -> a
    ssucc x | x == maxBound = minBound
            | otherwise = succ x

    spred :: a -> a
    spred x | x == minBound = maxBound
            | otherwise = pred x

-------------------------------------------------------------------------------
-- show и read
-------------------------------------------------------------------------------
-- show преобразует объект, относящийся к классу типов Snow, в строку
str_123 = show 123
-- read выполняет обратную операцию
num_123 = read "123" :: Integer
-- в случае неуспеха, read выдает исключение.

-- Есть еще одна функция: reads. Она возвращает список. Если парсинг неудачен, то список будет пустым.
reads_2girls = reads "2 girls" :: [(Int, String)]

-------------------------------------------------------------------------------
-- Форсированные вычиления
-------------------------------------------------------------------------------
{-
seq :: a -> b -> b
seq | b = |         -- если первый агрумент расходится, то функция должна расходиться
seq a b = b         -- в остальных случаях возвращается второй аргумент
-}

-- seq форсирует вычислени своего первого аргумента до слабой заголовочной нормально формы

trySeq1 = seq undefined 2
-- вернет расходящееся вычисление.

trySeq2 = seq (undefined, undefined) 2
-- вычислет первый аргумент до слабой заголовочной нормальной формы (в данном случае она будет
-- совпадать с (undefined, unfedined)) и вернет второй аргумет.

-- В функции quux seq позволяет сразу раскрывать длинные цепочки выди ((((n - 1) - 1) - 1) ... - 1)
quux 0 (x, y) = x + y
quux n (x, y) = let x' = x + 1
                    y' = y - 1
                    p  = (x', y')
                    n' = n - 1
                in x' `seq` y' `seq` n' `seq` quux n' p

-- Строгий оператор применения (аппликация с вызовом по значению)
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x         -- x, вычисленный заранее, используется при подстановке его в функию f

trySeq3 = const 1 $ undefined  -- undefined не вычисляется, вычисления не расходятся
trySeq4 = const 1 $! undefined -- undefined вычисляется, вычисления расходятся

-- Факториал без накапливающихся вычислений (пример)
fact'''' n | n >= 0 = accumulate 1 n
           | otherwise = error "Argument of factorial can not be negative"
    where
        accumulate acc 0 = acc
        accumulate acc n = (accumulate $! (acc * n)) (n-1)

-------------------------------------------------------------------------------
-- Списки
-------------------------------------------------------------------------------
-- Создание пустого списка
emptyList = []
-- Добавление элемента в начало списка
list3 = 3 : []
-- head возвращает первый элемент списка
exampleHead = head [1, 2, 3]
-- tail возвращает вес список без первого элемента
exampleTail = tail [1, 2, 3]

-- Длина списка
length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length(xs)

-- Конкатенация двух списков
(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : xs ++ ys

-- Функция, проверяющая, является ли список пустым
null :: [a] -> Bool
null [] = True
null _  = False

-- Функция, возвращающая список нечетных чисел из заданного списка (пример)
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
                | otherwise = oddsOnly xs

-- Последний элемент списка
last :: [a] -> a
last (x:[])  = x
last (_:xs) = last xs

-- Список без последнего элемента
init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs

-- Обращение списка
reverse :: [a] -> [a]
reverse l = rev l [] where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)

-- Упаковка двух списков в список из пар
zip :: [a] -> [b] -> [(a,b)]
zip []      _      = []
zip as      []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs

-- Упаковка трех списков в список из троек
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []

-- Распаковка списка из пар в два списка
unzip :: [(a,b)] -> ([a], [b])
unzip []        = ([], [])
unzip ((x,y):xys) =
    let (xs,ys) = unzip xys
    in (x:xs, y:ys)

-- Взять первые n элементов списка
take :: Int -> [a] -> [a]
take n _   | n <= 0 = []
take _ []           = []
take n (x:xs)       = x : take (n-1) xs

-- Пропустить первые n элементов списка
drop :: Int -> [a] -> [a]
drop n xs      | n <= 0 = xs
drop _ []               = []
drop n (_:xs)           = drop (n-1) xs

-- Разделить список на два подсписка
splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

-- Обращение к элементу списка по номеру
xs     !! n | n < 0 = error "NegativeIndex"
[]     !! _ = error "Index too large"
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
   | p x       = x :takeWhile p xs
   | otherwise = []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')            -- @ - это симсол синонима (псевдонима)
   | p x       = dropWhile p xs'
   | otherwise = xs

-- span
span :: (a -> Bool) -> [a] -> ([a],[a])
span p xs = (takeWhile p xs, dropWhile p xs)

-- break
break :: (a -> Bool) -> [a] -> ([a],[a])
break p = span (not . p)

-- map
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- concat
concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

-- concatMap
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = concat (map f xs)

-- all
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- any
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []     _      = []
zipWith _ _      []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-------------------------------------------------------------------------------
-- Генераторы списков
-------------------------------------------------------------------------------
-- Бесконечный список единиц
ones = 1 : ones

-- Возрастающие числа
nats n = n : nats (n+1)

-- Бесконечный список квадратов натуральных чисел
squareNats = map (^ 2) $ nats 1

-- Набор чисел Фибоначчи (пример)
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- repeat
{-
repeat :: a -> [a]
repeat x = xs where xs = x : xs
-}

-- replicate
{-
replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)
-}

-- cycle
{-
cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = ys where ys = xs ++ ys
-}

-- iterate
{-
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
-}

-- Арифмитические последовательности
-- Последовательность от 1 до 10
arifmSequence1 = [1..10]
-- что то же самое, что и
arifmSequence1' = enumFromTo 1 10
-- Последовательность с шагом 2
arifmSequence2 = [1,3..10]
arifmSequence2' = enumFromThenTo 1 3 10
-- Можно генерировать и бесконечные списки
arifmSequence3 = [1..]
arifmSequence3' = enumFrom 1
-- Или с заданным шагом
arifmSequence4 = [1,3..]
arifmSequence4' = enumFromThen 1 3

-- Выделение списков, генераторы
-- Список квадратов чисел от 1 до 10
list4 = [x^2 | x <- [1..10]]

list5 = [x^2 | x <- [1..10], x^2 < 50]

list6 = [(x, y) | x <- [1,2], y <- [1,2]]

-------------------------------------------------------------------------------
-- Свертки
-------------------------------------------------------------------------------
-- Правая свертка
-- Пример: 1 'f' (2 'f' (3 'f' (... 'f' ini)))
{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini []     = ini
foldr f ini (x:xs) = x `f` foldr f ini xs
-}

-- Левая свертка
-- ((((ini 'f' 1) 'f' 2) 'f' 3) ...)
{-
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini []     = ini
foldl f ini (x:xs) = foldl f (f ini x) xs
-}
-- Такую версию левой свертки обычно называют нестрогой. Ее минус в том,
-- что она создает большое отложенное вычисление вида f(f(f(...))).

-- Строгая версия левой свертки (из модуля Data.List)
{-
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f ini []     = ini
foldl' f ini (x:xs) = ini' `seq` foldl' f ini' xs
    where ini' = f ini x
-}

-- Правая свертка. Последний элемент списка является инициализирующим.
{-
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]    = x
foldr1 f (x:xs) = f x (foldr 1 f xs)
foldr1 _ []     = error "foldr1: EmptyList"
-}

-- Левая свертка. Первый элемент списка является инициализирующим.
{-
foldl1 :: (Ord a) => [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 _ [] = error "foldl1: EmptyList"
-}

-- snanl - левая свертка со всеми промежуточными значениями (from Data.List)
{-
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f ini []     = [ini]
scanl f ini (x:xs) = ini : scanl f (ini 'f' x) xs
-}

-- scanr - правая свертка с промежуточными значениями
{-
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ ini []      = [ini]
scanr f ini (x:xs) = (x 'f' q) : qs
                     where qs@(q:_) = scanr f ini xs
-}

-- unfold - генерация списка на основе значения
{-
unfold :: (b -> (a,b)) -> b -> [a]
unfold f ini = let (x, ini') = f ini in
    x : unfold f ini'
-}

-- Тип данных Maybe
-- find :: (a -> Bool) -> [a] -> Maybe a
found = Data.List.find odd [0, 2, 4]  -- = Nothing

-- unfoldr
{-
unfoldr (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
    helper (Just (x,ini')) = x : unfoldr f ini'
    helper Nothing         = []
-}

-------------------------------------------------------------------------------
-- Типы данных
-------------------------------------------------------------------------------
-- Конструктор типа = Конструктор данных 1 | Конструктор данных 2
data B = T | F deriving (Show, Read, Eq, Enum)
-- это является типом суммы

-- Функция, определенная на данных пользовательского типа
not' :: B -> B
not' T = F
not' F = T


-- Типы произведения
data Point = Point Double Double deriving Show

origin :: Point
origin = Point 0.0 0.0

distance (Point x1 y1) (Point x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)


-- Типы суммы призведений
data Roots = Roots Double Double | None deriving Show

squareRoots'' a b c
    | d >= 0 = Roots x1 x2
    | otherwise = None
    where
        x1 = (-b - sqrtD) / (2 * a)
        x2 = (-b + sqrtD) / (2 * a)
        d = b^2 - 4 * a * c
        sqrtD = sqrt (d)


-- Ленивые образцы.
-- В таком случае сопоставление с образцом всегда удачно и происходит только тогда
-- когда это становится необходимым для вычислений.
(***) f g ~(x,y) = (f x, g y)
-- Поэтому возможно произвести такой вызов:
example1 = ((const 1) *** (const 2)) undefined  -- = (1,2)


-- Метки полей
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show, Eq)

vasya = Person "Vasiliy" "Smith" 25
xavier = Person {age = 40, firstName = "Phideaux", lastName = "Xavier"}
-- unknownBill = Person {firstName = "Bill"} -- warning: не все поля заполнены

-- Доступ по метке
vasyasAge1 = age vasya
--vasyasAge2 = vasya & age   -- в новом стандарте
updateAge newAge person = person {age = newAge}

-- Сопоставления с образцами в синтаксисе записей
name (Person fn ln _) = fn ++ " " ++ ln
name' (Person {lastName = ln, firstName = fn}) = fn ++ " " ++ ln


-- Типы с параметрами
data CoordD = CoordD Double Double
data CoordI = CoordI Int Int
data Coord a = Coord a a

-- Тип данных Maybe
-- Maybe a = Noting | Just a

-- Тип Either
-- Either a b = Left a | Right b
squareRoots''' :: Double -> Double -> Double -> Either [Char] (Double, Double)
squareRoots''' a b c
    | d >= 0 = Right (x1, x2)
    | otherwise = Left "Negative discriminant"
    where
        x1 = (-b - sqrtD) / (2 * a)
        x2 = (-b + sqrtD) / (2 * a)
        d = b^2 - 4 * a * c
        sqrtD = sqrt (d)


-- Ленивые и строгие параметры
data CoordLazy a = CoordLazy a a        -- ленивые параметры
data CoordStrict a = CoordStrice !a !a  -- строгие параметры (вычисления этих параметров форсируются)


-- Инфиксные конструкторы данных.
-- Любой инфиксный конструктор данных должен начинаться с :
{-
data Complex a = !a :+ !a
data Ratio a   = !a :% !a
data [] a      = [] | a : ([] a)
-}

data List a = Nil | Cons a (List a)
    deriving Show


-- Синонимы типов
-- type String = [Char]
type IntegerList = [Integer]
type Map keyT valT = [(keyT, valT)]

-- newtype
newtype IntList = IList [Int] deriving Show
example2 = IList [1,2]
-- В отличие от type исползование newtype не наследует всех представителей классов типов.

-- Отличие newtype от data
-- - newtype гарантрованно имеет только один конструктор (это позволяет делать разные оптимизации)
-- - тип данных, определенный с помощью newtype, более ленив, чем тип, определенный с помощью data.

-- Такой контейнер присутсвует только во время разработаки.
-- Во время исполнения вместо него хранится просто значения типа a.
newtype Identity a = Identity {runIdentity :: a}
    deriving (Eq, Ord, Show)


-- Моноиды
class Monoid a where
    mempty :: a            -- нейтральный элемент
    mappend :: a -> a -> a -- бинарная операция

    mconcat :: [a] -> a    -- свертка
    mconcat = foldr mappend mempty

{- LAWS:
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-}

-- Списки - простейшие представители класса типов Monoid
instance Monoid [a] where
    mempty = []
    mappend = (++)

-- Числа относительно операций сложения и умножения являются монойдами.
-- Числа как моноид относительно операции сложения:
newtype Sum a = Sum {getSum :: a}
    deriving (Eq, Ord, Read, Show, Bounded)
-- Напоминание: такой контейнер существует только во время разработки.
-- Во время исполнения вместо него подставляется просто значение типа a.

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)

-- Числа как моноид относительно операции умножения:
newtype Product a = Product {getProduct :: a}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)


-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------
{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b    -- :kind f = * -> *, т.е. f - контейнерный тип
-}

{- LAWS
(1)    fmap id = id
(2)    fmap (f . g) = fmap f . fmap g
-}

-- fmap является обобщением функции map для контейнера f.
{-
instance Functor [] where
    fmap = map
-}

-- fmap можно определить и на контейнере Maybe
{-
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just a) = Just (f a)
-}

-- Говорят, что fmap поднимает вычисление в контейнер.
-- У fmap есть эквивалентный оператор <$>
example3 = (+1) <$> [1, 2, 3] -- = [2, 3, 4]

-------------------------------------------------------------------------------
-- Определение монады
-------------------------------------------------------------------------------
{-
-- Функции с эффектами
f :: a -> Maybe b        -- иногда может завершиться неудачей
f :: a -> [b]            -- может возвращать 0, 1 или много результатов
f :: a -> (Either s) b   -- иногда может завершаться типизированным исключением
f :: a -> (s,b)          -- может делать записи в лог (s - запись в лог)
f :: a -> ((->) e) b     -- может читать из внешнего окружения
f :: a -> (State s) b    -- работает с изменяемым состоянием
f :: a -> IO b           -- осуществляет ввод/вывод (файлы, консоль)

-- Все эти функции имеют вид, называемый стрелкой Клейсли:
f :: a -> m b
-}

{-
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (*>)  :: f a -> f b -> f b
    (<*)  :: f a -> f b -> f a

class Applicative m => Monad m where
    -- упаковка в контейнер
    return :: a -> m a
    -- связывание (оператор bind)
    (>>=) :: m a -> (a -> m b) -> m b
    -- облегченное связывание
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    -- функция fail
    fail :: String -> m a
    fail s = error s

{- LAWS:
return a >>= k   =  k a
m >>= return     =  m
(m >>= k >>= k') = m >>= (\x -> k x >>= k')
-}

-- Оператор монадического вычисления в обратную сторону
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

-- Оператор композиции монадических вычисление
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \x -> g x >>= f
-}

toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli f x = return (f x)

-------------------------------------------------------------------------------
-- Монада Identity
-------------------------------------------------------------------------------
-- Напоминание:
{-
newtype Identity a = Identity {runIdentity :: a}
    deriving (Eq, Ord, Show)
-}

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity v = Identity (f v)

instance Monad Identity where
    return x = Identity x
    Identity x >>= k = k x

-- do-нотация
{-
do { e1 ; e2 } = e1 >> e2
do { p <- e1; e2 } = e1 >>= \p -> e2
do { let v = e1; e2 } = let v = e1 in do e2
-}

wrap'n'succ x = Identity (succ x)

-- монадические вычисления goWrap1, goWrap2, goWrap3, goWrap4 эквивалентны:

goWrap1 =
    let i = 3 in
    wrap'n'succ 3 >>=
    wrap'n'succ >>=
    wrap'n'succ >>=
    return

goWrap2 =
    wrap'n'succ 3 >>= (\x ->
    wrap'n'succ x >>= (\y ->
    wrap'n'succ y >>= (\z ->
    return z)))

goWrap3 =
    let i = 3 in
    wrap'n'succ i >>= \x ->
    wrap'n'succ x >>= \y ->
    wrap'n'succ y >>
    return (i, x+y)

goWrap4 = do
    let i = 3
    x <- wrap'n'succ i
    y <- wrap'n'succ x
    wrap'n'succ y
    return (i, x+y)

-------------------------------------------------------------------------------
-- Монада Maybe
-------------------------------------------------------------------------------
{-
data Maybe a = Nothing | Just a
    deriving (Eq, Ord)

instance Monad Maybe where
    return x = Just x

    (Just x) >>= k = k x
    Nothing >>= _  = Nothing

    (Just _) >> m = m
    Nothing >> _  = Nothing

    fail _ = Nothing
-}

-------------------------------------------------------------------------------
-- Монада списка
-------------------------------------------------------------------------------
{-
instance Monad [] where
    return x = [x]
    xs >>= k = concatMap k xs
    fail   _ = []
-}

example4 = return 4 :: [Int]             -- = [4]
example5 = [1, 2] >>= (\x -> [x+1, x*2]) -- = [2,2,3,4]
example6 = [1, 2] >>= (\x -> [])         -- = []

-- Генератор списка list7 полностью эквивалентен монадному вычислению list7'
list7  = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
list7' = do
    x <- [1,2,3]
    y <- [4,5,6]
    return (x,y)

list8  = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
list8' = do
    x <- [1,2,3]
    y <- [4,5,6]
    True <- return (x /= y)
    return (x,y)

-------------------------------------------------------------------------------
-- Монада IO
-------------------------------------------------------------------------------
{-
type IO a = RealWorld -> (RealWorld, a)

instance Monad IO where
    return a = \w -> (w, a)
    (>>=) m k = \w -> case m w of (w',a) -> k a w'
-}

hello = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Nice to meet you, " ++ name ++ "!"

getLine' :: IO String
getLine' = do
    c <- getChar
    if c == '\n' then
        return []
    else do
        cs <- getLine'
        return (c:cs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

{-
sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())
-}

{-
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM f = sequence_ . map f
-}

{-
sequence :: Monad m => [m a] -> m [a]
sequence ms = foldr k (return ()) ms where
    k :: Monad m => m a -> m [a] -> m [a]
    k m m' = do
        x <- m
        xs <- m'
        return (x:xs)
-}

{-
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f
-}

-------------------------------------------------------------------------------
-- Монада Reader
-------------------------------------------------------------------------------
{-
instance Monad ((->) e) where
    -- return :: a -> (e -> a)
    return x = \_ -> x
    (>>=) :: (e -> a) -> (a -> e -> b) -> e -> b
    m >>= k = \e -> k (m e) e
-}

safeHead = do
    b <- null
    if b then
        return Nothing
    else do
        h <- head
        return $ Just h

example7  = safeHead []    -- = Nothing
example7' = safeHead [1,2] -- = Just 1

{-
newtype Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
    return x = Reader $ \e -> x
    m >>= k = Reader $ \e ->
        let v = runReader m e
        in runReader (k v) e

-- ask возвращает окружение
ask :: Reader r r
ask = Reader id

-- asks применяет функцию к окружению
asks :: (r -> a) -> Reader r a
asks = Reader

-- local выполняет временное локальное преобразование окружения
local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)

-- reader делает то же самое, что и Reader
reader :: (r -> a) -> Reader r a
reader f = do
    r <- ask
    return (f r)
-}

-------------------------------------------------------------------------------
-- Монада Writer
-------------------------------------------------------------------------------
{-
newtype Writer w a = Writer {runWriter :: (a, w)}

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    m >>= k =
        let (x,u) = runWriter m
            (y,v) = runWriter $ k x
        in Writer (y, u `mappend` v)

example8 = runWriter (return 3 : Writer String Int) -- = (3, "")

-- tell записывает в лог переданное значение
tell :: Monoid w => w -> Writer w ()
tell w = writer ((), w)

-}

-------------------------------------------------------------------------------
-- Монада State
-------------------------------------------------------------------------------
{-
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return a = State $ \st -> (a, st)

    m >>= k = State $ \st ->
        let (a, st') = runState m st
            m' = k a
        in runState m' st'

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put st = State $ \_ -> ((), st)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- (пример)
tick = do
    n <- get
    put (n+1)
    return n
-}

-------------------------------------------------------------------------------
-- Пример использования монады State
-------------------------------------------------------------------------------
-- Пример использования монады State для хранения состояния.
-- Пусть есть тип двоичного дерева, содержащего значения в узлах:
data Tree a = Leaf a | Fork (Tree a) a (Tree a)
    deriving Show

-- Требуется пронумеровать вершины дерева данной формы,
-- обойдя их in-order (то есть, сначала обходим левое поддерево,
-- затем текущую вершину, затем правое поддерево).

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (walkthrough tree) 1

walkthrough :: Tree () -> State Integer (Tree Integer)
walkthrough (Fork left _ right) = do
    leftBranch <- walkthrough left
    n <- get
    put (n+1)
    rightBranch <- walkthrough right
    return (Fork leftBranch n rightBranch)

walkthrough (Leaf _) = do
    n <- get
    put (n+1)
    return (Leaf n)


example9 = numberTree (Leaf ())
example10 = numberTree (Fork (Leaf ()) () (Leaf ()))

-------------------------------------------------------------------------------
-- Трансформеры монад
-------------------------------------------------------------------------------
secondElem :: Reader [String] String
secondElem = do
    el2 <- asks (map toUpper . head .tail)
    return el2

-- > runReader secondElem ["1", "2", "3"]
-- "2"

logFirst :: [String] -> Writer String String
logFirst xs = do
    let el1 = head xs
    let el2 = (map toUpper . head .tail) xs
    tell el1
    return el2

-- > runWriter $ logFirst ["a", "b", "c"]
-- ("B","a")

logFirstAndRetSecond :: ReaderT [String] -- трансформер
                        (Writer String) -- внутренняя монада
                        String -- тип, возвращаемый композитной монадой
logFirstAndRetSecond = do
    el1 <- asks head
    el2 <- asks (map toUpper . head .tail)
    lift $ tell el1 -- поднимаем вычисления из внутренней монады
    return el2

-- > :t runReaderT logFirstAndRetSecond
-- runReaderT logFirstAndRetSecond :: [String] -> Writer String String
-- > runWriter $ runReaderT logFirstAndRetSecond ["a", "b", "c"]
-- ("B","a")

logFirstAndRetSecond2 :: WriterT String (Reader [String]) String
logFirstAndRetSecond2 = do
    el1 <- lift $ asks head
    el2 <- lift $ asks (map toUpper . head .tail)
    tell el1
    lift $ return el2

-- > (runReader (runWriterT logFirstAndRetSecond2)) ["a", "b", "c"]
-- ("B","a")

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 ss = do
    let log1 = filter p1 ss
    let log2 = filter p2 ss
    let ret = filter (\x -> not (p1 x) && not (p2 x)) ss
    tell log1
    lift $ tell log2
    return ret

-- > (runWriter . runWriterT) $ separate (<3) (>7) [0..10]
-- (([3,4,5,6,7],[0,1,2]),[8,9,10])

-------------------------------------------------------------------------------
-- MyRW - My Reader-Writer
-------------------------------------------------------------------------------
type MyRW = ReaderT [String] (Writer String)
-- тогда можно записать logFirstAndRetSecond :: MyRW String

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter (runReaderT rw e)

-- > runMyRW logFirstAndRetSecond ["a", "b", "c"]
-- ("B","a")

-- asks можно вызывать напрямую, но tell - только с помощью lift.
-- Поэтому напишем свои функции для доступа к интерфейсу монады MyRW
-- myAsks :: ([String] -> a) -> MyRW a
-- myAsks = asks
-- myTell :: String -> MyRW ()
-- myTell = lift . tell

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rw e = runWriterT (runReaderT rw e)

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

logFirstAndRetSecondIO :: MyRWT IO String
logFirstAndRetSecondIO = do
    el1 <- myAsks head
    myLift $ putStrLn $ "First is " ++ show el1
    el2 <- myAsks (map toUpper . head . tail)
    myLift $ putStrLn $ "Second is " ++ show el2
    myTell el1
    return el2

-- > runMyRWT logFirstAndRetSecondIO ["a", "b", "c"]
-- First is "a"
-- Second is "B"
-- ("B","a")

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- reader :: Monad m => (r -> a) -> ReaderT r m a
-- reader f = ReaderT (return . f)
