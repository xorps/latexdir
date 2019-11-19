module Number

[<AbstractClass>]
type Number() =
    abstract member Int: int

module Types =
    type _1() =
        inherit Number()
        override _.Int = 1

    type _2() =
        inherit Number()
        override _.Int = 2

    type _3() =
        inherit Number()
        override _.Int = 3

    type _4() =
        inherit Number()
        override _.Int = 4

    type _5() =
        inherit Number()
        override _.Int = 5

    type _10() =
        inherit Number()
        override _.Int = 10

    type _12() =
        inherit Number()
        override _.Int = 12

module Values =
    let _1 = Types._1()
    let _2 = Types._2()
    let _3 = Types._3()
    let _4 = Types._4()
    let _5 = Types._5()
    let _10 = Types._10()
    let _12 = Types._12()
