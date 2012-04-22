#!/usr/bin/env python
import os
import sys
# i=10
# pid = os.fork()
# if pid == 0:
#     print 'Glad to meet ya'
#     i=15
#     tpid = os.fork()
#     if tpid == 0:
#         print 'I am the female weezy'
#         i=23
#         #os.execvp('ls',['/home'])
#     else:
#         cpid,bits = os.waitpid(tpid,0)
#         assert i == 15
#         print 'tchild pid: ' + str(cpid)
#         print 'texit code: ' + str(bits >> 8)
# else:
#     cpid,bits = os.waitpid(pid,0)
#     assert i==10
#     print 'this is i ' + str(i)
#     print 'child pid: ' + str(cpid)
#     print 'exit code: ' + str(bits >> 8)
# if pid == 0:
#     print 'child finishing'
#     exit(1)
# else:
#     print 'parent finishing'
#     sys.exit(1)

def pipetothree():
  pidlist = []
  p1_r,p1_w = os.pipe()
  p2_r,p2_w = os.pipe()
  p3_r,p3_w = os.pipe()
  def closer(p): os.close(p)
  def readpipe(p):
      pid = os.fork()
      if pid != 0:
          pidlist.append(pid)
      else:
          map(closer,[p1_w,p2_w,p3_w])
          while True:
              stuff = os.read(p,8192)
              if not stuff: 
                  os.close(p)
                  break
              print stuff
      return pid

  for pr,pw in [(p1_r,p1_w),(p2_r,p2_w),(p3_r,p3_w)]:
      if readpipe(pr):
          os.close(pr)
      else:
          print 'child finished'
          exit(1)

  toclose = [p1_w,p2_w,p3_w]
  print 'parent writing'
  while True:
      out=raw_input("")
      if out == "exit": break
      map(lambda p: os.write(p,out), toclose)
  print 'parent finished'

  for i,pid in enumerate(pidlist):
      os.close(toclose[i])
      os.waitpid(pid,0)
      print 'child %d finished' % pid

pipetothree()  


        

  
