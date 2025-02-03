import regex
# ^ keyword
#        ^ namespace

import regex.v1
# ^ keyword
#        ^ namespace
#            ^ namespace

import regex as rxp
# ^ keyword
#        ^ namespace
#            ^ keyword
#                ^ namespace

import regex.v1 as rxp
# ^ keyword
#        ^ namespace
#               ^ keyword
#                   ^ namespace

import ...regex
# ^ keyword
#           ^ namespace