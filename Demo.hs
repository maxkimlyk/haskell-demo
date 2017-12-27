--------------------------------------------------------
--    __ __         __       ____  ___                --
--   / // /__ ____ / /_____ / / / / _ \___ __ _  ___  --
--  / _  / _ `(_-</  '_/ -_) / / / // / -_)  ' \/ _ \ --
-- /_//_/\_,_/___/_/\_\\__/_/_/ /____/\__/_/_/_/\___/ --
--------------------------------------------------------

-- Это демо модуль на языке Haskell, в котором показаны основные особенности возможности языка.
--
-- Материал берется в основном отсюда:
-- http://wiki.nsunc.com/_export/html/haskell
-- и отсюда:
-- https://stepik.org/course/75/

-------------------------------------------------------------------------------
-- Полезные команды оболочки ghci
-------------------------------------------------------------------------------

-- :q, :quit     - выйти
-- :l, :load     - загрузить программу
-- :r, :reload   - перезагрузить текущий модуль
-- :t, :type     - напечатать сигнатуру функции
-- :i, :info     - напечатать сигнатуру функции и указать, в каком файле функция была определена
-- :?, :h, :help - help

-- let используется, чтобы определять новые функции:

-- > let {f :: Int -> Int; f n = n * 2}

-- > :{
-- | let f :: Int -> Int
-- |     f n = n * 2
-- > :}

-------------------------------------------------------------------------------
-- Модули
-------------------------------------------------------------------------------
-- Это название модуля, определяемого в данном файле
module Demo where

-- Импортирование других модулей
import Data.Char
import Data.Complex

-------------------------------------------------------------------------------
-- Определение функций
-------------------------------------------------------------------------------
-- У функции может быть явно задана сигнатура, но это не обязательно.
fact :: Integer -> Integer
-- Шаблонное определение функции
fact 0 = 1
fact n = n * fact (n-1)

-- или через гарды
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
            (_, 0) -> x
            (_, n) -> 1 + casing x (n-1)

-------------------------------------------------------------------------------
-- Частичное применение функций, каррирование
-------------------------------------------------------------------------------
add x y = x + y

-- Частичное применение
inc = add 1

-- Каррирование.
-- fst принимает двухэлементный кортеж,
-- curry fst принимает уже 2 аргумента.

-- uncarry выполняет ровно обратное действие.
-------------------------------------------------------------------------------
-- Лямбды
-------------------------------------------------------------------------------
inc' = (\x y -> x + y) 1
add' = \x y -> x + y

-------------------------------------------------------------------------------
-- Префиксный и постфиксный стиль
-------------------------------------------------------------------------------
-- Функции можно вызывать как в префиксном так и в постфиксном стилях
max1 = max 10 28
max2 = 10 `max` 28

-- И опраторы можно вызывать как в префиксном так и в постфиксном стилях
sum1 = 10 + 28
sum2 = (+) 10 28
-- единственное исколючение - унарным минус
-- (-) 10 - это частично применненный бинарный оператор вычитания
-- (-10) - а это число -10

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
-- (*+*) a b = a ^ 2 + b ^ 2

-- Для частичного применения операторов есть специальный синтаксис (сечения)
twoDivBy = (2 /)
divByTwo = (/ 2)

-------------------------------------------------------------------------------
-- Левая и правая ассициативность операторов
-------------------------------------------------------------------------------
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
-- По сути этот оператор ничего не делает:
infixr 0 &
f & x = f x
-- но, имея приоритет 0, он позволяет избежать избыточного применения скобок:
-- sin (pi / 2)
-- sin $ pi / 2

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

-- fst возвращает первый элемент двухэлементного кортежа
cort1 = fst cort
-- snd возвращает второй элемент двухэлементного кортежа
cort2 = snd cort

-- или пустой кортеж
emptyCort = ()


-- Списки
list1 = [1, 2, 3]
list2 = [True, False, False]
-- список типа Char:
str1 = "hello"

-- : - оператор добавления элемента в голову списка
str2 = 'W' : "elcome"

-- (++) - оператор конкатенации списков
str3 = "Wel" ++ "come";

-------------------------------------------------------------------------------
-- error и undefined
-------------------------------------------------------------------------------
-- error "Message" - прерывает выполнение программы и выводит сообщение "Message"
-- undefined - прерывает выполнение программы и выводит стандартное сообщение об ошибке

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
-- id x = x
-- const x y = x
-- flip f y x = f x y
-- f . g = \x -> f (g x)

-------------------------------------------------------------------------------
-- Классы типов
-------------------------------------------------------------------------------
-- Класс типов, для которых определен оператор не равно.
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

