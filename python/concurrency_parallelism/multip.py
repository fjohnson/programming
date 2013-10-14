import multiprocessing

exit = False

def f(rlist):
    print rlist,

def done(arg):
    print arg
    global exit
    exit = True

if __name__ == '__main__':
    print 'Main process started ...'
    #multiprocessing.Pool(2).map_async(f,range(100),chunksize=5,callback=done)


    nitems,nprocs = 4,2
    v = range(nitems)
    cz = nitems/nprocs
    procs = []
    start = 0
    end = cz
    remainder = nitems % nprocs
    for i in range(0,nprocs):
        procs.append(multiprocessing.Process(target=f,args=(v[start:end],)))
        start = end
        end += cz
    if remainder:
            end -= cz
            procs.append(multiprocessing.Process(target=f,args=(v[end:end+remainder],)))
    for p in procs: p.start()
    for p in procs: p.join()

