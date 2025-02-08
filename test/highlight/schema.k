schema Person:
#^ @keyword
#        ^ type
    age: int = 33
    # ^ variable
    #     ^ type
    #          ^ number

schema Employee(Person):
#^ @keyword
#        ^ type
#               ^ type
    salary: float = 1000.0
    # ^ variable
    #       ^ type
    #               ^ number

protocol DataProtocol:
#^ @keyword
#         ^ type
    data: str
    # ^ variable
    #     ^ type

weather = "sunny"
day = "wednesday"

rule IsSunny:
#^ @keyword
#      ^ type
    weather == "sunny"
    # ^ variable
    #       ^ operator
    #            ^ string

rule IsWednesday:
    day == "wednesday", "today is not wednesday"
    # ^ variable
    #   ^ operator
    #         ^ string
    #                     ^ string

rule Main(IsSunny, IsWednesday):
#^ @keyword
#      ^ type
#           ^ type
#                   ^ type
    id == 1