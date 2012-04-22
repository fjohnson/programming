import threading
import time
import os

die=1
def msgcount(lock,name):
   global die 
   rollcount = 6 
   while rollcount:
       lock.acquire()
       print 'I am %s and this is the roll of the die ...' % name
       print die
       die += 1
       lock.release()
       rollcount -= 1

   
# print 'PID: %d' % os.getpid()
# lock = threading.RLock()
# t1=threading.Thread(None,msgcount,'t1',(lock,'t1'))

# t2=threading.Thread(None,msgcount,'t2',(lock,'t2'))
# t2.start()
# t1.start()
# t1.join()
# t2.join()

iolist = []
def prodcom(role,semaphore,name,produce=None):
    global iolist
    if role != "producer":
        start = time.time()
    while True:
        if role != "producer":time.sleep(1)
        semaphore.acquire()
        if role == "producer":
            if produce: 
                print 'producing ' + str(produce[0])
                iolist.append(produce[0])
                produce = produce[1:]
            else:
                print 'production finished. exiting.'
                semaphore.release()
                return
        else:
            if iolist:
                print name + ' consuming ' + str(iolist[0])
                iolist = iolist[1:]
            if time.time() - start > 10:
                print name + ' consumed for 10 sec. exiting'
                semaphore.release()
                return
        semaphore.release()

# print 'PID: %d' % os.getpid()
# semaphore = threading.BoundedSemaphore()
# t1=threading.Thread(None,prodcom,'t1',("producer",semaphore,'t1'),{"produce":range(30)})
# t2=threading.Thread(None,prodcom,'t2',("consumer",semaphore,'t2'))
# t3=threading.Thread(None,prodcom,'t3',("consumer",semaphore,'t3'))
# t3.start()
# t2.start()
# t1.start()
# t1.join()
# t2.join()
# t3.join()

def gg(condition):
    if threading.current_thread().name == "t1":
        time.sleep(1)
        condition.acquire()
        condition.notify()
        condition.release()
        print 'notifier ended'
    else:
        condition.acquire()
        condition.wait()
        condition.release()
        print threading.current_thread().name + ' finished waiting'
    
print 'PID: %d' % os.getpid()
condition = threading.Condition()
t1=threading.Thread(None,gg,'t1',(condition,))
t2=threading.Thread(None,gg,'t2',(condition,))
t1.start()
t2.start()
t1.join()
t2.join()
