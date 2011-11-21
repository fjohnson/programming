import sys
from subprocess import Popen, PIPE

class commandError(Exception): 
  '''Denotes a syntax error in shell command'''
  def __init__(self, message):
      if message == "BAD <":
          message = "Incorrect usage of <\n"
          message += "Correct usage is 'command [args] < file'\n"
      elif message == "BAD &":
          message = "Incorrect usage of &\n"
          message += "Correct usage is 'pipeline&\n'"
      elif message == "BAD 2&>":
          message = "Incorrect usage of 2&>\n"
          message += "Correct usage is command 2&> file"
      elif message == "BAD >":
          message = "Incorrect usage of >\n"
          message += "Correct usage is command > file"

      Exception.__init__(self, message)
                             
def parseCommand(command):
  pipelines = filter(lambda x : x, command.split(';'))
  pipe_retcode = None
  for pl in pipelines: 
    commands = parsePipeline(pl)
    pipe_retcode = chainCommands(commands)
  if pipe_retcode != None: return pipe_retcode

def getRestrictedTokens():
  restricted = ['<','2&>','>', '&'] #order matters here.
  return restricted

def checkControl(arg):
  '''Check an apparent command argument for pipeline control tokens.
  raises a commandError if the argument is malformed (contains ctokens)
  returns True if the arg is infact a token
  returns False if the arg is simply just an argument'''
  restricted = getRestrictedTokens()
  for rtoken in restricted:
      if rtoken in arg: 
          #arg is a control token.
          if arg == rtoken: return True
          #control token is present in an arg.
          raise commandError("Incorrect usage of %s token" % rtoken)
  #arg is a bonafied argument.
  return False 

def executeCommand(command, pipe=None, newpipe=False):
  '''Execute a single command.
  command is a list of arguments appropriate for Popen
  pipe is a communication pipe provided by a previous command.
  newpipe indicates that the output of this command should be piped.
  return the exit code of this command if its output is not piped.
  otherwise return a pipe for the next process to use.'''

  args,controlcs = command
  #stderr output is always placed on the screen.
  #stdout on the otherhand may be piped.
  err = sys.stderr
  if newpipe: sout = PIPE
  else: sout = sys.stdout

  
  if '2&>' in controlcs:
    outfile = open(args.pop(),'w')
    p = Popen(args, stdin = pipe, stdout = outfile, stderr = outfile)
  elif '>' in controlcs:
    outfile = open(args.pop(),'w')
    p = Popen(args, stdin = pipe, stdout = outfile, stderr = err)
  elif '<' in controlcs:
    infile = open(args.pop(),'r')
    p = Popen(args, stdin = infile, stdout = sout, stderr = err)
  else:
    p = Popen(args, stdin = pipe, stdout = sout, stderr = err)

  if newpipe:
   return p

  if '&' in controlcs:
    return p.wait()

def chainCommands(parsedCommands):
    '''the idea here is to begin setting up processes.
    the parsedcommands is a list of 2-tuples. each tuple
    contains ([program,args...,args...],[&,>,etc.])
    looking at the seen control characters should immediately
    indicate how to do the linking. i.e if seen controls is empty of
    redirections then chainging is done with a pipe'''
    print parsedCommands
    n = len(parsedCommands)
    pipe = None
    for i,p in enumerate(parsedCommands):
      #Don't create a new pipe. Last command in the chain.
      if i == n - 1:  
        ret = executeCommand(p, pipe)
        
        #close reading side of pipe so writer of the pipe
        #can get a SIGPIPE if this process ends before it is
        #finished.
        if pipe: pipe.close() 
      else: 
        p = executeCommand(p, pipe, newpipe=True)
        if pipe: pipe.close()
        pipe = p.stdout
        
def tokenizeCommand(command):
  '''Split a command up extracting its tokens.
  Tokens are delimited by spaces quoting spaces or escaping them is
  allowed. only \' quotes are supported.
  Return the tokens in a list, just like str.split()
  '''

  #print command
  tokens = []
  escaped = False
  quoted = False 
  currentToken = []

  for c in command:
    if quoted:
      if c == "'": 
        quoted = False
        continue
      currentToken.append(c)
      continue
    elif escaped: 
      escaped = False
      currentToken.append(c)
      continue

    if c == '\\':escaped = True
    elif c == "'": quoted = True
    elif c == ' ': #split tokens on spc 
      if currentToken: #don't add empty tokens.
        tokens.append(''.join(currentToken))
        currentToken = []
    else: currentToken.append(c)

  if quoted or escaped: #end quote or lone escape
    raise commandError("Bad command %s command\n"+
                       "Lone escape or missing end quote.\n")
  if currentToken: #command had token not ending in a space.
    tokens.append(''.join(currentToken))
  return tokens
    

def parsePipeline(pipeline):
    pipeline = pipeline.strip()
    
    if pipeline[-1] == '&':
      pipeline_wait = True
      pipeline = pipeline[:-1]
    else:
      pipeline_wait = False

    commands = pipeline.split('|')
    parsedCommands = []
    for command in commands:
        seenControls = []

        args = []
        tokens = tokenizeCommand(command)
        for i,arg in enumerate(tokens):
            isControl = checkControl(arg)
            #executable was a control character!
            if isControl and not arg:
                raise commandError("BAD %s" % arg)
            #arg was a command arg
            if not isControl: args.append(arg)

            if isControl:
                #arg is a control char but it has already been seen. 
                if arg in seenControls:
                    raise commandError("BAD %s" % arg)
                
                if arg == '>' or arg == '2&>' or arg == '<':
                    #more than one file provided or no file provided.
                    #this check also prevents more than a single instance
                    #of > or 2&> or < 
                    if i != len(tokens) - 2:
                        raise commandError("BAD %s" % arg)
                    #file redirection must be in the very first command.
                    if arg == '<' and parsedCommands:
                      raise commandError("BAD %s" % arg)
                    
                    args.append(tokens[i+1])
                    seenControls.append(arg)
                    break
                
        parsedCommands.append((args,seenControls))

    if pipeline_wait: 
      parsedCommands[-1][1].append('&')
    return parsedCommands


        
  
