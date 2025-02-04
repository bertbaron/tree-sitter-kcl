schema Person:
#^ @keyword
#        ^ type
    age: str = 33
    # ^ variable
    #     ^ type
    #          ^ number

protocol DataProtocol:
#^ @keyword
#         ^ type
    data: str
    # ^ variable
    #     ^ type

rule SomeRule:
#^ @keyword
#      ^ type
    age > 0, "rule check failure message"
    # ^ variable
    #   ^ operator
    #     ^ number
    #         ^ string