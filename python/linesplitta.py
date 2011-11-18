import os
import sys

def compose(f,y):
  def ret(*x):
    inner_result = y(*x)
    return f(inner_result) #f(y(x))
  return ret
#y = lambda x,y,z : x + y + z
#f= lambda x : x * 1000
#c = compose(f,y)

#Take args, return function where you pass
#remaining args to call

def partial_apply(f, *args):
  def f_reduct(*next_args):
    return f(* (args + next_args))
  return f_reduct

def quote_string(string):
  return '"'+string+'"'

def splitdaline(line,width=50):
    
    if len(line) < width: return quote_string(line)
    
    current_line = quote_string(line[:width])
    next_line = splitdaline(line[width:],width)    
    return current_line + " + " + os.linesep + next_line
            

if __name__ == '__main__':
  if len(sys.argv) > 2:
    print splitdaline(sys.argv[1], sys.argv[2])
  else:
    print splitdaline(sys.argv[1])


