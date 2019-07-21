	program	nicegrid2

!-----------------------------------------------------------------------
!
!	AUTHOR:		Andre Fortunato - 2009-11-18 (interior nodes)
!                       Nicolas Bruneau - 2010-04-20 (iopbnd=1)
!                       Andre Fortunato - 2018-06-18 (iopbnd=2)
!
!	PURPOSE:	improve a FE grid
!			The goal is to:
!			1 - prevent interior nodes that belong to only 3
!			    or 4 elements
!			2 - keep the number of elements per interior 
!			    node as close to 6 as possible. The mean 
!			    deviation to 6 is indicated at the end of 
!			    the run for the initial and the final grids
!                       3 - Remove problem (angle > 90) for the 
!                           boundary elements
!
!                       A description of an earlier version of the code
!                       is given in:
!                       Fortunato, AB, N Bruneau, A Azevedo, MAVC Araujo,
!                       A Oliveira (2011). Automatic improvement of 
!                       unstructured grids for coastal simulations, Journal
!                       of Coastal Research, Special Issue 64, 1028-1032
!
!       COMPILATION:    gfortran -o nicegrid2 nicegrid2.f90
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD),h(MXNOD)
	real*8		xf(MXNOD),yf(MXNOD),hf(MXNOD)
	integer*4	neigh(MXNOD,MNEI)
	integer*4	ncon(MXEL,3),ic3(MXEL,3)
	integer*4	ine(MXNOD,MNEI)
	integer*4	nneigh(MXNOD),ib(MXNOD),nne(MXNOD),iflagn(MXNOD)
	integer*4	nto(MNBOU),ntc(MNBOU),ntcindex(MNBOU)
	integer*4	iobn(MNBOU,NEBND),icbn(MNBOU,NEBND)
	integer*4	nbouo,nbouc,ntto,nttc,ibound
	integer*4	i,j,n,nodes,nel,neimin,neimax,iflag,nodesf

	real*4		avneii,avneif
	integer*4	nodesi,neli,neimini,neiminf,neimaxi,neimaxf, &
        &               nodesin,nodesinf,iabort,iter,itermax

	integer*4	iargc
	character*40	file,file2,file3

        integer*4       BND(NEBND,MNBOU)
        integer*4       iBND,iopbnd
	logical		found
!-----------------------------------------------------------------------
!  -  help
	if (iargc()/=3) then
	    write(*,*)'Program nicegrid2'
	    write(*,*)'Purpose: improve a triangular FE grid'
	    write(*,*)'format:  nicegrid2 gridin gridout iopbnd'
            write(*,*)'  iopbnd: 0 - do not change boundary'
            write(*,*)'          1 - correct boundary as well'
            write(*,*)'              (bnd info needed in grid)'
            write(*,*)'          2 - correct and adjust boundary'
            write(*,*)'              (bnd info needed in grid)'
	    write(*,*)
	    write(*,*)'If iopbnd!=0, the input grid must have the'
	    write(*,*)'boundary info'
	    write(*,*)'If nicegrid.flag (bpt file) exists, only nodes'
	    write(*,*)'with flag=1 will be changed, and an updated'
	    write(*,*)'file (nigrid.flag.out) is produced'
	    stop
	endif

!  -  read from screen
	call getarg(1,file)
        call rgrid(file,nel,nodes,x,y,h,ncon,               &
     &             iobn,icbn,nto,ntc,ntcindex,nbouo,nbouc,  &
     &             ntto,nttc,ibound)
        
	call getarg(3,file)
        read(file,*)iopbnd
        if (iopbnd<0 .or. iopbnd>2) then
            write(*,*)'Incorrect value for iopbnd. Stop'
            stop
        endif
        if (iopbnd>0 .and. ibound == 0) then
            write(*,*)'iopbnd>0 but bo boundary is provided. stop'
            stop
        else
!       Global boundary vector
          Do i=1,nbouo
             BND(1,i) = nto(i)
             Do j=1,nto(i)
                BND(j+1,i) = iobn(i,j)
             Enddo
          Enddo
          Do i=1,nbouc
             BND(1,i+nbouo) = ntc(i)
             Do j=1,ntc(i)
                BND(j+1,i+nbouo) = icbn(i,j)
             Enddo
          Enddo
          iBND = nbouo+nbouc
        endif

	call getarg(2,file)

!  -  check the existence of nicegrid.flag. If it exists, only nodes with
!     iflagn=1 will be moved
	file3 = 'nicegrid.flag'
	inquire (file=file3,exist=found)
	if (found) then
	    iflag = 0
	    call rbpt(file3,xf,yf,hf,nodesf)
	    if (nodes/=nodesf) then
		write(*,*)' Number of nodes in nicegrid.flag is incorrect:'
		write(*,*)nodesf,'instead of ',nodes
		stop
	    endif
	    do i = 1, nodes
		iflagn(i) = hf(i)
	    end do
	else
	    iflag = 1
	    do i = 1, nodes
		iflagn(i) = 1
	    end do
	endif

!  -  Initialize. itermax is the maximum number of iterations over the whole 
!     loop. This is used to avoid infinite loops.
	iter = 1
	itermax = 5

!  -  Compute grid info
	write(*,*)'Compute grid information'
	call neighb(x,y,nel,nodes,nneigh,neigh,neimin,neimax,ncon)
	call tables(ic3,ine,ncon,nne,nodes,nel)
	call bnodes(ncon,nneigh,ib,nodes,nel)

!  -  Save initial grid parameters
	nodesi = nodes
	neli   = nel
	neimini= 100
	neimaxi= 0
	nodesin= 0
	avneii = 0.
	do i = 1, nodes
            if (ib(i)==0) then
		neimini = min(nneigh(i),neimini)
		neimaxi = max(nneigh(i),neimaxi)
		avneii  = avneii+abs(nneigh(i)-7)
		nodesin = nodesin+1
	    endif
	end do
	neimini = neimini-1
	neimaxi = neimaxi-1
	avneii  = avneii/float(nodesin)

! Boundary change
        if (iopbnd>0)          &
        &  call CorBND(x,y,iBND,BND,nneigh,neigh,ine,ncon,nne,  &
        &              nodes,nel,ib,ic3,neimin,neimax,iflag,iflagn)

!  -  Deal with nodes that belong to 3 elements
1	call nodes3or4(x,y,ncon,ic3,neigh,ine,nne,ib,nneigh,    &
        &              nodes,nel,3,iopbnd,iBND,BND,iflag,iflagn)

!  -  Check need for another iteration
!	do i = 1, nodes
!	    if (nne(i)==3 .and. ib(i)==0
!     #		.and. iflagn(i)==0) goto 1
!	end do

!  -  Deal with nodes that belong to 4 elements
2	call nodes3or4(x,y,ncon,ic3,neigh,ine,nne,ib,nneigh,    &
        &              nodes,nel,4,iopbnd,iBND,BND,iflag,iflagn)

!  -  Check need for another iteration
!	do i = 1, nodes
! 	    if (nne(i)==3 .and. ib(i)==0
!     #          .and. iflagn(i)==0) goto 1
!	end do
!	do i = 1, nodes
!	    if (nne(i)==4 .and. ib(i)==0) goto 2
!	end do

!  -  Deal with nodes that belong to 5 elements
	call nodes5(x,y,ncon,ic3,neigh,ine,nne,ib,nneigh,       &
        &           nodes,nel,neimin,neimax,                    &
        &           iopbnd,iBND,BND,iflag,iflagn)

!  -  Check need for another iteration
!	do i = 1, nodes
!	    if (nne(i)==3 .and. ib(i)==0
!     #		.and. iflagn(i)==0) goto 1
!	end do
!	do i = 1, nodes
!	    if (nne(i)==4 .and. ib(i)==0) goto 2
!	end do

!  -  Deal with nodes that belong to 8 elements
	call nodes8(x,y,ncon,ic3,neigh,ine,BND,nne,ib,nneigh,   &
        &           nodes,nel,neimin,neimax,iflag,              &
        &           iflagn,iBND,iopbnd)

!  -  Check if there are nodes that belong to over 8 elements:
!     nicegrid can't handle those
	iabort = 0
	do i = 1, nodes
	    if (nne(i)>8) then
		write(*,*)' Node',i,'belongs to',nne(i),'elements'
		write(*,*)' Correct manually'
		iabort = 1
	    endif
	end do

!  -  Check need for another iteration
	if (iabort==0 .and. iter .le. itermax) then
	    iter = iter+1
	    do i = 1, nodes
		if (nne(i)==4 .and. ib(i)==0 .and.      &
        &           iflagn(i)==0) goto 2
	    end do
	endif

!  -  Check areas and correct if required
	call calc_areas(x,y,ncon,nodes,nel)

!  -  Spring
	call springs_bnd(x,y,neigh,BND,nneigh,ib,50,nodes,    &
        &                iflag,iflagn,iBND,iopbnd)

!  -  Save final grid parameters
	neiminf = 100
	neimaxf = 0
	nodesinf= 0
	avneif = 0.
	do i = 1, nodes
	    if (ib(i)==0) then
		neiminf  = min(nneigh(i),neiminf)
		neimaxf  = max(nneigh(i),neimaxf)
		avneif   = avneif+abs(nneigh(i)-7)
		nodesinf = nodesinf+1
	    endif
	end do
	neiminf = neiminf-1
	neimaxf = neimaxf-1
	avneif  = avneif/float(nodesinf)

	write(*,*)
	write(*,*)
	write(*,*)'SUMMARY OF GRID CHANGES'
	write(*,*)
	write(*,*)'                   Initial grid  Final Grid   % Change'
	write(*,100)' Nodes:            ',nodesi,nodes,100.*    &
        &	    float(nodes-nodesi)/float(nodesi)
	write(*,100)' Elements:         ',neli,nel,100.*        &
        &	    float(nel-neli)/float(neli)
	write(*,100)' Neighb. (min):    ',neimini,neiminf,100.* &
        &	    float(neiminf-neimini)/float(neimini)
	write(*,100)' Neighb. (max):    ',neimaxi,neimaxf,100.* &
        &	    float(neimaxf-neimaxi)/float(neimaxi)
	write(*,110)' Neighb (aver dev):',avneii,avneif,100.*   &
        &	    (avneif-avneii)/avneii
100	format(A22,i6,5x,i6,10x,f5.1)
110	format(A22,2x,f4.2,7x,f4.2,9x,f6.1)

!  -  write grid
        if (iopbnd>0) then
!       Global boundary vector
          Do i=1,nbouo
             nto(i) = BND(1,i)
             Do j=1,nto(i)
                iobn(i,j) = BND(j+1,i)
             Enddo
          Enddo
          nbouc = iBND-nbouo
          Do i=1,nbouc
             ntc(i) = BND(1,i+nbouo)
             Do j=1,ntc(i)
                icbn(i,j) = BND(j+1,i+nbouo)
             Enddo
          Enddo
        endif
        call wgrid(file,nel,nodes,x,y,h,ncon,               &
     &		   iobn,icbn,nto,ntc,ntcindex,nbouo,nbouc,  &
     &		   ntto,nttc,ibound)

	if (iflag==0) then
	    file = 'nicegrid.flag.out'
	    call wbpt(file,x,y,iflagn,nodes)
	endif

!  -  perform final checks and issue warnings, if needed
	write(*,*)'Perform final quality checks'
	call checks(x,y,ib,ncon,nel,nodes)

	end

!-----------------------------------------------------------------------
	subroutine	nodes3or4(x,y,ncon,ic3,neigh,ine,nne,ib,nneigh,&
        &                         nodes,nel,itof,iopbnd,iBND,BND,iflag,&
        &                         iflagn)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-11-18
!
!	PURPOSE:	improve a grid by eliminating interior nodes 
!			that belong to only 3 or 4 elements (itof)
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD)
	integer*4	ncon(MXEL,3),ic3(MXEL,3)
	integer*4	ine(MXNOD,MNEI)
	integer*4	neigh(MXNOD,MNEI)
	integer*4	nneigh(MXNOD),ib(MXNOD),nne(MXNOD),iflagn(MXNOD)
	integer*4	nielnext(MNEI),nnenext(MNEI)
	integer*4	ielnext(MNEI),iel(MNEI)
	integer*4	niel(2)
	integer*4	i,j,k,l,nodes,nel,iela,nodeielnext,itof,       &
        &               nnemin,nodemin,neimin,neimax,icountkill,       &
        &               icountswap,ielmin,ielnextmin,jmin,ine_ij,      &
        &               ine_next,iflag
        integer*4       BND(NEBND,MNBOU),iBND,it,jj,iopbnd
!-----------------------------------------------------------------------
!  -  Verify itof
	if (itof .ne.3 .and. itof/=4) then
	    write(*,*)' Incorrect itof',itof
	    stop
	endif

	icountkill = 0
	icountswap = 0

!  -  search and correct
	i = 0
	do while (i<nodes)
	    i = i+1
	    if (iflagn(i)==1) then
	    	if (ib(i)==0 .and. nne(i)==itof) then
!  -  find nodes and elements around the ball surrounding node i
		    call search(ine,ic3,ncon,nielnext,nne,nnenext,iel, &
        &                       ielnext,itof,i)

!  -  find adjacent interior node that has the minimum number of elements
		    nnemin = 100
		    jmin   = 0
		    do j = 1, nne(i)
		    	if (nielnext(j)/=0) then	!avoid boundary segments
		    	  if (nnenext(j)<nnemin .and. ib(nielnext(j))==0) then
			    jmin   = j
			    nnemin = nnenext(j)
		    	  endif
		    	endif
		    end do
!  -  Kill the node or swap lines
		    if (nnemin>=6 .or. jmin==0) then
		        write(*,*)' Kill:',i
		        call killnode(x,y,ine,ncon,neigh,nne,i,nodes,nel, &
        &                             0,ib,iflag,iflagn)
                    	if (iopbnd > 0) Then
                     	    print*, 'KILL NODE',i
                     	    Do jj = 1,iBND
                      	      Do it = 2,BND(1,jj)+1
                      	        if(BND(it,jj).ge.i) BND(it,jj)=BND(it,jj)-1
                      	      Enddo
                     	    Enddo
                    	endif
		    	icountkill = icountkill+1
		    	i = i-1
		    else
		    	nodemin= nielnext(jmin)
		    	ielmin = iel(jmin)
		    	ielnextmin = ielnext(jmin)
			if (iflag+                              &
        &                   iflagn(ncon(ielmin,1))+             &
        &                   iflagn(ncon(ielmin,2))+             &
        &                   iflagn(ncon(ielmin,3))==0) then
			  write(*,*)' Kill:',i
			  call killnode(x,y,ine,ncon,neigh,nne,i,nodes,nel, &
        &                               0,ib,iflag,iflagn)
		    	  icountkill = icountkill+1
		    	  i = i-1
			else
		    	  write(*,*)' Swap:',ielmin,ielnextmin
			  call swaplines(ncon,ielmin,ielnextmin,i,nodemin)
		    	  icountswap = icountswap+1
			endif
		    endif
!  -  Update grid parameters
		    if (icountkill+icountswap/=0) then
		        call neighb(x,y,nel,nodes,nneigh,neigh,neimin, &
        &                           neimax,ncon)
		        call tables(ic3,ine,ncon,nne,nodes,nel)
		        call bnodes(ncon,nneigh,ib,nodes,nel)
		    endif
	        endif
	    endif
	end do

!  -  Spring and report
	call springs_bnd(x,y,neigh,BND,nneigh,ib,5,nodes,    &
        &                iflag,iflagn,iBND,iopbnd)
	call report(itof,icountkill,0,icountswap)

	return
	end

!-----------------------------------------------------------------------
	subroutine	nodes5(x,y,ncon,ic3,neigh,ine,nne,ib,nneigh,   &
        &                      nodes,nel,neimin,neimax,iopbnd,iBND,BND,&
        &                      iflag,iflagn)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-11-18
!
!	PURPOSE:	improve a grid by eliminating interior nodes 
!			that belong to only 5 elements
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD)
	integer*4	ncon(MXEL,3),ic3(MXEL,3)
	integer*4	ine(MXNOD,MNEI)
	integer*4	neigh(MXNOD,MNEI)
	integer*4	nneigh(MXNOD),ib(MXNOD),nne(MXNOD),iflagn(MXNOD)
	integer*4	nielnext(MNEI),nnenext(MNEI)
	integer*4	ielnext(MNEI),iel(MNEI)
	integer*4	niel(2)
	integer*4	i,j,k,l,nodes,nel,iela,ielnextmin,nnemin,      &
        &               nodemin,sumnne,neimin,neimax,icountkill,       &
        &               icountswap,ielj,nei5,neinext,isumnei,n5,n6,n7, &
        &               nei55,iflag
        integer*4       BND(NEBND,MNBOU),iBND,it,jj,iopbnd
!-----------------------------------------------------------------------
	icountkill = 0
	icountswap = 0
	icountkill = 0
	icountswap = 0
!  -  search and correct adjoining balls with two fives
	do i = 1, nodes
	  if (iflagn(i)==1) then
	    if (ib(i)==0 .and. nne(i)==5) then
!  -  find nodes and elements around the ball surrounding node i
		call search(ine,ic3,ncon,nielnext,nne,nnenext,iel,     &
        &                   ielnext,5,i)

!  -  find adjacent interior node that has 5 elements, and swap if worth it
	        sumnne = 1
		do j = 1, nne(i)
		    if (nnenext(j)==5 .and. ib(nielnext(j))==0) then

!  -  find nodes from element iel (niel(2)) that are not i
		    	l = 0
		    	do k = 1, 3
			    if (ncon(iel(j),k)/=i) then
				l = l+1
				niel(l) = ncon(iel(j),k)
			    endif
		    	end do		!k
			if (nne(niel(1))+nne(niel(2))>sumnne) then
			    sumnne     = nne(niel(1))+nne(niel(2))
			    nodemin    = nielnext(j)
			    ielnextmin = ielnext(j)
			    ielj       = iel(j)
			endif
		    endif
		end do			!j
		if (sumnne>12) then

		    if (iflagn(ncon(ielj,1))+   &
        &               iflagn(ncon(ielj,2))+   &
        &               iflagn(ncon(ielj,3))>=2) then
!  -  swap lines
			write(*,*)' Swap:',ielj,ielnextmin
			call swaplines(ncon,ielj,ielnextmin,i,nodemin)
			icountswap = icountswap+1
!  -  Update grid parameters
		    	call neighb(x,y,nel,nodes,nneigh,neigh,neimin, &
        &                           neimax,ncon)
		    	call tables(ic3,ine,ncon,nne,nodes,nel)
		    	call bnodes(ncon,nneigh,ib,nodes,nel)
		    endif
	    	end if
	    endif
	  endif
	end do				!i

!  -  search and correct adjoining balls with one five and one six
	do i = 1, nodes
	  if (iflagn(i)==1) then
	    if (ib(i)==0 .and. nne(i)==5) then
!  -  find nodes and elements around the ball surrounding node i
		call search(ine,ic3,ncon,nielnext,nne,nnenext,iel,  &
&                           ielnext,5,i)

!  -  find adjacent interior node that has 6 elements, and swap if worth it
	        sumnne = 1
		do j = 1, nne(i)
		    if (nnenext(j)==6 .and. ib(nielnext(j))==0) then

!  -  find nodes from element iel (niel(2)) that are not i
		    	l = 0
		    	do k = 1, 3
			    if (ncon(iel(j),k)/=i) then
				l = l+1
				niel(l) = ncon(iel(j),k)
			    endif
		    	end do		!k
			if (nne(niel(1))+nne(niel(2))>sumnne) then
			    sumnne     = nne(niel(1))+nne(niel(2))
			    nodemin    = nielnext(j)
			    ielnextmin = ielnext(j)
			    ielj       = iel(j)
			endif
		    endif
		end do			!j
		if (sumnne>13) then

!  -  swap lines
		    if (iflagn(i)*iflagn(nodemin)==1) then
		      write(*,*)' Swap:',ielj,ielnextmin
		      call swaplines(ncon,ielj,ielnextmin,i,nodemin)
		      icountswap = icountswap+1
		    endif
!  -  Update grid parameters
		    call neighb(x,y,nel,nodes,nneigh,neigh,neimin,neimax,ncon)
		    call tables(ic3,ine,ncon,nne,nodes,nel)
		    call bnodes(ncon,nneigh,ib,nodes,nel)
	    	end if
	    endif
	  endif
	end do				!i

!  -  Find two adjoining nodes with 5 elements surrounded only by nodes
!     with 6 elements, and delete one of the nodes
	i = 0
	do while (i<nodes)
	  i = i+1
	  if (iflagn(i)==1) then
	    if (ib(i)==0 .and. nne(i)==5) then
!  -  check if there is one and only one surrounding i with 5 elements
		call check5(i,nneigh,neigh,n5,n6,n7,nei5)
		if (n5==1 .and. n6==2 .and. n7==2) then
		     call check5(nei5,nneigh,neigh,n5,n6,n7,nei55)
		     if (n5==1 .and. n6==2 .and. n7==2) then
!  -  kill node i
			write(*,*)' Kill:',i
			call killnode(x,y,ine,ncon,neigh,nne,i,nodes,nel, &
        &                             nei5,ib,iflag,iflagn)

                        if (iopbnd > 0) Then
                         print*, 'KILL NODE',i
                         Do jj = 1,iBND
                          Do it = 2,BND(1,jj)+1
                           if(BND(it,jj)>=i) BND(it,jj)=BND(it,jj)-1
                          Enddo
                         Enddo
                        Endif

			icountkill = icountkill+1
			i = i-1

!  -  Update grid parameters
			call neighb(x,y,nel,nodes,nneigh,neigh,neimin, &
        &                           neimax,ncon)
			call tables(ic3,ine,ncon,nne,nodes,nel)
			call bnodes(ncon,nneigh,ib,nodes,nel)
		     endif
		endif
	    endif
	  endif
	end do

!  -  Find two adjoining nodes with 5 elements surrounded only by nodes
!     with 6 elements, and delete one of the nodes
!	i = 0
!	do while (i<nodes)
!	    i = i+1
!	    if (ib(i)==0 .and. nne(i)==5) then
!c  -  check if there is one and only one surronding node with 5 elements
!		nei5 = 0
!		isumnei = 0
!		do j = 2, nneigh(i)
!		    neinext = neigh(i,j)
!		    isumnei = isumnei+nneigh(neinext)
!		    if (ib(neinext)==1) goto 1
!		    if (nneigh(neinext)==6 .and. nei5/=0) goto 1
!		    if (nneigh(neinext)==6) nei5 = neinext
!		end do
!		if (isumnei==34) goto 1	!all are 6s
!
!c  -  Check if all nei5 neighbours have >= 6 elements but i
!	if(i.eq.109)write(*,*)i,neinext
!		do j = 2, nneigh(nei5)
!	if(i.eq.109)write(*,*)nneigh(neigh(nei5,j)),neigh(nei5,j)
!		    if (ib(neigh(nei5,j))==1) goto 1
!		    if (nneigh(neigh(nei5,j))<7 .and.
!     #			neigh(nei5,j)/=i) goto 1
!		end do
!c  -  kill node i
!		write(*,*)' Kill:',i
!		call killnode(x,y,ine,ncon,neigh,nne,i,nodes,nel)
!		icountkill = icountkill+1
!		i = i-1
!
!  -  Update grid parameters
!		call neighb(x,y,nel,nodes,nneigh,neigh,neimin,neimax,ncon)
!		call tables(ic3,ine,ncon,nne,nodes,nel)
!		call bnodes(ncon,nneigh,ib,nodes,nel)
!1		continue
!	    endif
!	end do				!i

!  -  Spring and report
	call springs_bnd(x,y,neigh,BND,nneigh,ib,5,nodes,    &
        &                iflag,iflagn,iBND,iopbnd)
	call report(5,icountkill,0,icountswap)

	return
	end

!-----------------------------------------------------------------------
	subroutine	nodes8(x,y,ncon,ic3,neigh,ine,BND,nne,ib,nneigh,&
        &                      nodes,nel,neimin,neimax,iflag,iflagn, &
        &                      iBND,iopbnd)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-12-04
!
!	PURPOSE:	improve a grid by eliminating interior nodes 
!			that belong to 8 elements
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD)
	integer*4	ncon(MXEL,3),ic3(MXEL,3)
	integer*4	ine(MXNOD,MNEI)
	integer*4	neigh(MXNOD,MNEI)
        integer*4       BND(NEBND,MNBOU)
	integer*4	nneigh(MXNOD),ib(MXNOD),nne(MXNOD),iflagn(MXNOD)
	integer*4	ielkill(2),nodef(2)
	integer*4	iel,iBND
	integer*4	i,j,k,l,m,nodes,nel,nnemax,nodemax,icountadd,  &
        &               neimin,neimax,nnemin,iflag,iopbnd
!-----------------------------------------------------------------------
!  -  search and correct
	icountadd = 0
	i = 0
	do while (i<nodes)
	    i = i+1
	    if (iflagn(i)==1) then
!	      if (ib(i)==0 .and. nne(i)==8) then
	      if (ib(i)==0 .and. nne(i)>=8) then
!  -  find neighbour node with max number of elements
		nnemax = 0
		nnemin = 10
		do j = 2, nneigh(i)
		    if (nneigh(neigh(i,j))>nnemax .and.         &
        &               ib(neigh(i,j))==0) then
			nnemax = nneigh(neigh(i,j))
			nodemax= neigh(i,j)
		    endif
		    if (nneigh(neigh(i,j))<nnemin .and.         &
        &               ib(neigh(i,j))==0) then
			nnemin = nneigh(neigh(i,j))
		    endif
		end do
		if (nnemin>=7) goto 1
!  -  Find elements to delete and corresponding nodes
		l = 0
		do j = 1, nne(i)
		    iel = ine(i,j)
		    do k = 1, 3
			if (ncon(iel,k)==nodemax) then
			    l = l+1
			    ielkill(l) = iel
			    do m = 1, 3
			      if (ncon(iel,m)/=nodemax .and. ncon(iel,m)/=i) &
                &                 nodef(l) = ncon(iel,m)
			    end do
			endif
		    end do
		end do
!  -  Add node
		write(*,*)' Add node between:',i,nodemax
		nodes = nodes+1
		if (nodes>MXNOD) then
		    write(*,*)'Maximum number of nodes exceeded'
		    stop
		endif
		x(nodes) = (x(i)+x(nodemax))/2.
		y(nodes) = (y(i)+y(nodemax))/2.
		iflagn(nodes) = 1
		icountadd= icountadd+1
!  -  delete and add elements
		if (nel+2>MXEL) then
		    write(*,*)' Maximum number of elements exceeded'
		    stop
		endif
		call delelem(ielkill,ncon,nel)
		ncon(nel+1,1) = nodes
		ncon(nel+1,2) = nodef(1)
		ncon(nel+1,3) = nodemax
		ncon(nel+2,1) = nodes
		ncon(nel+2,2) = nodemax
		ncon(nel+2,3) = nodef(2)
		ncon(nel+3,1) = nodes
		ncon(nel+3,2) = nodef(2)
		ncon(nel+3,3) = i
		ncon(nel+4,1) = nodes
		ncon(nel+4,2) = i
		ncon(nel+4,3) = nodef(1)
		nel = nel+4
!  -  Update grid parameters
		call neighb(x,y,nel,nodes,nneigh,neigh,neimin,neimax,ncon)
		call tables(ic3,ine,ncon,nne,nodes,nel)
		call bnodes(ncon,nneigh,ib,nodes,nel)
1		continue
	      endif
	    endif
	end do

!  -  Spring and report
	call springs_bnd(x,y,neigh,BND,nneigh,ib,5,nodes,    &
        &                iflag,iflagn,iBND,iopbnd)
	call report(8,0,icountadd,0)

	return
	end

!-----------------------------------------------------------------------
	subroutine	search(ine,ic3,ncon,nielnext,nne,nnenext,iel, &
        &                      ielnext,itof,nodei)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-11-18
!
!	PURPOSE:	Find nodes and elements around the ball surrounding
!			nodei
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	integer*4	ncon(MXEL,3),ic3(MXEL,3)
	integer*4	ine(MXNOD,MNEI)
	integer*4	nne(MXNOD)
	integer*4	nielnext(MNEI),nnenext(MNEI)
	integer*4	ielnext(MNEI),iel(MNEI)
	integer*4	niel(2)
	integer*4	nodei,j,k,l,iela,nodeielnext,nodemin,   &
        &               itof,ielmin,ielnextmin,ine_ij,ine_next
!-----------------------------------------------------------------------
	do j = 1, itof
	    ine_ij = ine(nodei,j)
!  -  find adjoining element (ielnext) that does not include node i
	    do k = 1, 3
		iela = ic3(ine_ij,k)
		if (iela==0) then		!boundary side:
		    nielnext(j)= 0
		    nnenext(j) = 0
		    ine_next   = 0
		    goto 1			!skip search
		endif
		if (ncon(iela,1)/=nodei .and. ncon(iela,2)/=nodei .and. &
        &           ncon(iela,3)/=nodei) ine_next = iela
	    end do		!k
!  -  find nodes from element iel (niel(2)) that are not nodei
	    l = 0
	    do k = 1, 3
		if (ncon(ine_ij,k)/=nodei) then
		    l = l+1
		    niel(l) = ncon(ine_ij,k)
		endif
	    end do		!k
!  -  find node (nodeielnext,nielnext(j)) that does not belong to element ine_ij
	    do k = 1, 3
		l = ncon(ine_next,k)
		if (l/=niel(1) .and. l/=niel(2)) nodeielnext = l
	    end do		!k
	    nielnext(j)= nodeielnext
	    nnenext(j) = nne(nodeielnext)
1	    iel(j)     = ine_ij
	    ielnext(j) = ine_next
	end do			!j

	return
	end

!-----------------------------------------------------------------------
	subroutine	springs_bnd(x,y,neigh,BND,nneigh,ib,iter,nodes,&
        &                           iflag,iflagn,iBND,iopbnd)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2018-06-20
!
!	PURPOSE:	spring a grid, including the boundary
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'

	real*8		x(MXNOD),y(MXNOD),xn(MXNOD),yn(MXNOD)
        real*8          OA(2),OB(2),NOA,NOB
        real*8          cosT,sinT,Theta,Theta2
	real*8		xsum,ysum,dtheta,that,tstar,x1,x2,y1,y2,xs,ys
	real*8		epsi
	integer*4	neigh(MXNOD,MNEI)
	integer*4	nneigh(MXNOD),ib(MXNOD),iflagn(MXNOD)
        integer*4       BND(NEBND,MNBOU)
	integer*4	iter,nodes,i,j,k,ii,iflag,bij,bim1j,bip1j
        integer*4       iBND,Nmax,iopbnd

!-----------------------------------------------------------------------
        dtheta = 2. !small angle (degrees) to define straigth boundaries
        epsi   = 1./3.
	do i = 1, iter
            xn = x; yn = y
	    do j = 1, nodes             !interior nodes
	    	if (iflagn(j)==1 .and. ib(j) == 0) then
		    xsum = 0.d0
		    ysum = 0.d0
		    do k = 2, nneigh(j)
			xsum = xsum+x(neigh(j,k))
			ysum = ysum+y(neigh(j,k))
		    end do
		    xn(j) = xsum/dfloat(nneigh(j)-1)
		    yn(j) = ysum/dfloat(nneigh(j)-1)
		endif
	    end do
            if (iopbnd == 2) then       !boundary nodes
                do j = 1, iBND
                    Nmax = BND(1,j)
                    do ii = 3, Nmax     !skip first and last
                        if (iflagn(BND(ii,j))==1) then
!               Calculate the angle...
                            bij   = BND(ii,j)
                            bim1j = BND(ii-1,j)
                            bip1j = BND(ii+1,j)
                            OA(1) = x(bim1j)-x(bij)
                            OA(2) = y(bim1j)-y(bij)
                            NOA   = dsqrt((OA(1))**2.+(OA(2))**2.)
                            OB(1) = x(bip1j)-x(bij)
                            OB(2) = y(bip1j)-y(bij)
                            NOB   = dsqrt((OB(1))**2.+(OB(2))**2.)
                            cosT  = (OA(1)*OB(1)+OA(2)*OB(2))/NOA/NOB
                            sinT  = (OA(1)*OB(2)-OB(1)*OA(2))/NOA/NOB
                            Theta = modulo(datan2(sinT,cosT)*180.       &
            &                       /(4.*atan(1.)),360.)   

!               Test angle and move node
                            if(dabs(Theta-180.) < dtheta) then
!               Stpring...
		                xsum = 0.d0
		                ysum = 0.d0
		                do k = 2, nneigh(bij)
			            xsum = xsum+x(neigh(bij,k))
			            ysum = ysum+y(neigh(bij,k))
		                end do
		                xs = xsum/dfloat(nneigh(bij)-1)
		                ys = ysum/dfloat(nneigh(bij)-1)
!               ...and project onto boundary
                                that = ((xs-x(bim1j))*(x(bip1j)-x(bim1j))+ &
        &                               (ys-y(bim1j))*(y(bip1j)-y(bim1j)))/&
        &                               (NOA+NOB)**2.
                                tstar= dmin1(dmax1(that,epsi),1.-epsi)
                                xn(bij) = x(bim1j)+tstar*(x(bip1j)-x(bim1j))
                                yn(bij) = y(bim1j)+tstar*(y(bip1j)-y(bim1j))
!                                xn(bij) = (x(bim1j)+x(bip1j))/2.d0
!                                yn(bij) = (y(bim1j)+y(bip1j))/2.d0
                            endif
                        endif
                    end do
                end do
            endif               !boundary nodes
	    do j = 1, nodes
		if (iflagn(j)==1) then
		    if (ib(j)==0 .or. (ib(j)==1 .and. iopbnd==2)) then
		        x(j) = xn(j)
		        y(j) = yn(j)
		    endif
		endif
	    end do
	end do

	return
	end

!-----------------------------------------------------------------------
	subroutine	rgrid(file,nel,nodes,x,y,h,ncon,               &
     &		   	      iobn,icbn,nto,ntc,ntcindex,nbouo,nbouc,  &
     &		   	      ntto,nttc,ibound)
     			      

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 95-04-14
!
!	PURPOSE:	read grid file in .gr3 format
!
!-----------------------------------------------------------------------
	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD),h(MXNOD)
	integer*4	ncon(MXEL,3)
	integer*4	nto(MNBOU),ntc(MNBOU),ntcindex(MNBOU)
	integer*4	iobn(MNBOU,NEBND),icbn(MNBOU,NEBND)
	integer*4	nbouo,nbouc,ntto,nttc,ibound
	character*40	file,alpha

	integer*4	i,j,n,nodes,nel
!-----------------------------------------------------------------------

	open(unit=1,file=file,status='old')
        read(1,20)alpha
20	format(a40)
        read(1,*)nel,nodes
        if (nel .gt. MXEL) then
            write(*,*)'Number of elements is too large:', nel
            stop
        elseif (nodes .gt. MXNOD) then
            write(*,*)'Number of nodes is too large:', nodes
            stop
        endif

        do i = 1, nodes
            read(1,*)n,x(i),y(i),h(i)
        end do
        do i = 1, nel
            read(1,*)n,n,(ncon(i,j),j=1,3)
        end do

	call rbound(iobn,icbn,nto,ntc,ntcindex,nbouo,nbouc,     &
     &		    ntto,nttc,ibound)

        close(1)

	return
	end
!=======================================================================
	subroutine	wgrid(file,nel,nodes,x,y,h,ncon,               &
     &		   	      iobn,icbn,nto,ntc,ntcindex,nbouo,nbouc,  &
     &			      ntto,nttc,ibound)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 95-04-14
!
!	PURPOSE:	write grid file in .gr3 format
!
!-----------------------------------------------------------------------
	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD),h(MXNOD)
	real*4		hh
	integer*4	ncon(MXEL,3)
	integer*4	nto(MNBOU),ntc(MNBOU),ntcindex(MNBOU)
	integer*4	iobn(MNBOU,NEBND),icbn(MNBOU,NEBND)
	integer*4	nbouo,nbouc,ntto,nttc,ibound
	character*40	file

	integer*4	i,j,nodes,nel
!-----------------------------------------------------------------------

	open(unit=1,file=file,status='unknown')
        write(1,20)'created by program bndepth'
20	format(a35,f3.1)
        write(1,*)nel,nodes

        do i = 1, nodes
	    hh = h(i)
            write(1,*)i,x(i),y(i),hh
        end do
        do i = 1, nel
            write(1,*)i,3,(ncon(i,j),j=1,3)
        end do

	if (ibound .eq. 1)                                             &
     &      call wbound(iobn,icbn,nto,ntc,ntcindex,nbouo,nbouc,        &
     &			ntto,nttc)
        close(1)

	return
	end

!=======================================================================
	subroutine	rbound(iobn,icbn,nto,ntc,ntcindex,nbouo,nbouc, &
     &			       ntto,nttc,ibound)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 96-02-12
!
!	PURPOSE:	read boundary nodes
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	integer*4	nto(MNBOU),ntc(MNBOU),ntcindex(MNBOU)
	integer*4	iobn(MNBOU,NEBND),icbn(MNBOU,NEBND)
	integer*4	nbouo,nbouc,i,k,ibound,ntto,nttc
!-----------------------------------------------------------------------
!	by default, assume that the boundary information exists
	ibound = 1
!...
!... Read open boundaries
!...
	read(1,*,end=1)nbouo
	if (nbouo .gt. MNBOU) then
	    write(*,*)' Number of open boundaries exceeds MNBOU'
	    stop
	endif
	read(1,*)ntto
	do i = 1, nbouo
	    read(1,*)nto(i)
            if (nto(i)>NEBND) then
                write(*,*)'nto(i)>NEBND',nto(i),NEBND
                stop
            endif
	    do k = 1, nto(i)
		read(1,*)iobn(i,k)
	    end do
	end do
!...
!... Read closed boundaries
!...
	read(1,*)nbouc
	if (nbouc .gt. MNBOU) then
	    write(*,*)' Number of closed boundaries exceeds MNBOU'
	    stop
	endif
	read(1,*)nttc
	do i = 1, nbouc
	    read(1,*)ntc(i),ntcindex(i)
            if (ntc(i)>NEBND) then
                write(*,*)'ntc(i)>NEBND',ntc(i),NEBND
                stop
            endif
	    do k = 1, ntc(i)
		read(1,*)icbn(i,k)
	    end do
	end do

	return

1	ibound = 0
	write(*,*)'There is no boundary information in the grid file'

	return
	end
!=======================================================================
	subroutine	wbound(iobn,icbn,nto,ntc,ntcindex,nbouo,nbouc, &
     &			       ntto,nttc)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 96-02-12
!
!	PURPOSE:	write boundary nodes
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	integer*4	nto(MNBOU),ntc(MNBOU),ntcindex(MNBOU)
	integer*4	iobn(MNBOU,NEBND),icbn(MNBOU,NEBND)
	integer*4	nbouo,nbouc,i,k,ibound,ntto,nttc
!-----------------------------------------------------------------------
!...
!... Write open boundaries
!...
	write(1,*)nbouo
	write(1,*)ntto
	do i = 1, nbouo
	    write(1,*)nto(i)
	    do k = 1, nto(i)
		write(1,*)iobn(i,k)
	    end do
	end do
!...
!... Write closed boundaries
!...
	write(1,*)nbouc
	write(1,*)nttc
	do i = 1, nbouc
	    write(1,*)ntc(i),ntcindex(i)
	    do k = 1, ntc(i)
		write(1,*)icbn(i,k)
	    end do
	end do

	return
	end

!-----------------------------------------------------------------------
        subroutine      CorBND(x,y,iBND,BND,nneigh,neigh,ine,ncon,nne, &
        &                      nodes,nel,ib,ic3,neimin,neimax,iflag,&
        &                      iflagn)
!-----------------------------------------------------------------------
!       AUTHOR:         Nicolas Bruneau - 2010-04
!
!       PURPOSE:        Correct boundary
!
!-----------------------------------------------------------------------
        include         'nicegrid2.cmn'
        real*8          x(MXNOD),y(MXNOD)
        real*8          OA(2),OB(2),NOA,NOB
        real*8          cosT,sinT,Theta,Theta2
        integer*4       ncon(MXEL,3),ic3(MXEL,3)
        integer*4       ine(MXNOD,MNEI)
        integer*4       NEIGH(MXNOD,MNEI)
        integer*4       NNEIGH(MXNOD),ib(MXNOD),nne(MXNOD),iflagn(MXNOD)
        integer*4       BND(NEBND,MNBOU)
        integer*4       nodes,nel,cc,compteur,iflag
        character*40    file
        integer*4       i,j,jj,it,k,iBND,Nmax,neimin,neimax
	integer*4	bij,bip1j,bim1j
!-----------------------------------------------------------------------
!       BND treatment
        cc=0
        Do j=1,iBND
          write(*,*)'Correct boundary',j
          i    = 3
          Nmax = BND(1,j)
          Do While (i <= Nmax)
	    if (iflagn(i)==1) then
              if (NNEIGH(BND(i,j))==4) then
!               Calculate the angle...
	        bij   = BND(i,j)
	        bim1j = BND(i-1,j)
	        bip1j = BND(i+1,j)
                OA(1) = x(bim1j)-x(bij)
                OA(2) = y(bim1j)-y(bij)
                NOA   = dsqrt((OA(1))**2.+(OA(2))**2.)
                OB(1) = x(bip1j)-x(bij)
                OB(2) = y(bip1j)-y(bij)
                NOB   = dsqrt((OB(1))**2.+(OB(2))**2.)
                cosT  = (OA(1)*OB(1)+OA(2)*OB(2))/NOA/NOB
                sinT  = (OA(1)*OB(2)-OB(1)*OA(2))/NOA/NOB
                Theta = modulo(datan2(sinT,cosT)*180./(4.*atan(1.)),360.)

!               Test angle and kill node
                if( Theta < 190. .and. Theta > 170.) then

                  if(i>3 .and. i<Nmax-1) then
                    x(bim1j)=2./3.*x(bim1j)+x(bij)/3.
                    y(bim1j)=2./3.*y(bim1j)+y(bij)/3.
                    x(bip1j)=2./3.*x(bip1j)+x(bij)/3.
                    y(bip1j)=2./3.*y(bip1j)+y(bij)/3.
                  endif

                  call killnode(x,y,ine,ncon,neigh,nne,bij,nodes,nel, &
        &                       0,ib,iflag,iflagn)

!             Adjust Boundary table
                  Nmax     = Nmax-1
                  BND(1,j) = Nmax  
                  Do it = i,Nmax+1
                    BND(it,j) = BND(it+1,j)
                  Enddo
                  Do jj = 1,iBND
                    Do it = 2,BND(1,jj)+1
                      if(BND(it,jj).gt.bij) BND(it,jj)=BND(it,jj)-1
                    Enddo
                  Enddo

!             Update grid parameters
                  call neighb(x,y,nel,nodes,nneigh,neigh,neimin,        &
        &                     neimax,ncon)
                  call tables(ic3,ine,ncon,nne,nodes,nel)
                  call bnodes(ncon,nneigh,ib,nodes,nel)
                  cc = cc+1
                endif
              endif
            endif
            i = i+1
          Enddo
        Enddo   
        Print*, 'Number of Boundary nodes deleted :',cc
   
        return
        end

!-----------------------------------------------------------------------
!******************************************************************************
!                                                                             *
!      Subroutine to generate a neighbor table from a connectivity table.     *
!                                                                             *
!      NOTE:the node itself is listed as neighbor #1                          *
!      NOTE:all other neighbors are sorted and placed in cw order from east   *
!                                                                             *
!                       R.L.       4/26/95                                    *
!******************************************************************************
!                                                                             *
!     -  PARAMETERS WHICH MUST BE SET TO CONTROL THE DIMENSIONING OF ARRAYS   *
!           ARE AS FOLLOWS:                                                   *
!                                                                             *
!          MXNODES = MAXIMUM NUMBER OF NODAL POINTS                           *
!          MNE = MAXIMUM NUMBER OF ELEMENTS                                   *
!          MNEI= 1+MAXIMUM NUMBER OF NODES CONNECTED TO ANY ONE NODE IN THE   *
!                   FINITE ELEMENT GRID                                       *
!                                                                             *
!******************************************************************************
!                                                                             *
!    VARIABLE DEFINITIONS:                                                    *
!       nel - NUMBER OF ELEMENTS                                              *
!       nodes - NUMBER OF NODES                                               *
!       NM(MNE,3) - NODE NUMBERS ASSOCIATED WITH EACH ELEMENT                 *
!       NNEIGH(MXNODES) NUMBER OF NEIGHBORS FOR EACH NODE                     *
!       NEIGH(MXNODES,NEIMAX) 2D ARRAY OF NEIGHBORS FOR EACH NODE             *
!       NEIMIN - 1+MINIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!       NEIMAX - 1+MAXIMUM NUMBER OF NEIGHBORS FOR ANY NODE                   *
!                                                                             *
!******************************************************************************
!
      SUBROUTINE NEIGHB(x,y,NEL,NODES,NNEIGH,NEIGH,NEIMIN,NEIMAX,NM)

      include	'nicegrid2.cmn'

      real*8	X(MXNOD),Y(MXNOD)
      integer*4	NM(MXEL,3)
      integer*4	NEIGH(MXNOD,MNEI)
      integer*4	NNEIGH(MXNOD)
      integer*4	NEITEM(MNEI)
      real*4	ANGLE(MNEI)
      real*4	rad2deg,delx,dely,dist,anglemore,anglelow
      INTEGER*4	EN1,EN2,EN3,n,nn,nodes,nel,neimin,neimax,nscreen,j,i,jj, &
        &       jlow

      nscreen = 1
      RAD2DEG=45./ATAN(1.)

      DO 5 N=1,nodes
         NNEIGH(N) = 0
         DO 5 NN=1,MNEI
            NEIGH(N,NN) = 0
   5        CONTINUE

      DO 10 N=1,nel
         EN1 = NM(N,1)
         EN2 = NM(N,2)
         EN3 = NM(N,3)
         DO 20 J=1,NNEIGH(EN1)
  20        IF(EN2.EQ.NEIGH(EN1,J)) GOTO 25
         NNEIGH(EN1)=NNEIGH(EN1)+1
         NNEIGH(EN2)=NNEIGH(EN2)+1
         IF((NNEIGH(EN1).GT.MNEI-1).OR.(NNEIGH(EN2).GT.MNEI-1)) GOTO 999
         NEIGH(EN1,NNEIGH(EN1))=EN2
         NEIGH(EN2,NNEIGH(EN2))=EN1
  25     DO 30 J=1,NNEIGH(EN1)
  30        IF(EN3.EQ.NEIGH(EN1,J)) GOTO 35
         NNEIGH(EN1)=NNEIGH(EN1)+1
         NNEIGH(EN3)=NNEIGH(EN3)+1
         IF((NNEIGH(EN1).GT.MNEI-1).OR.(NNEIGH(EN3).GT.MNEI-1)) GOTO 999
         NEIGH(EN1,NNEIGH(EN1))=EN3
         NEIGH(EN3,NNEIGH(EN3))=EN1
  35     DO 50 J=1,NNEIGH(EN2)
  50        IF(EN3.EQ.NEIGH(EN2,J)) GOTO 10
         NNEIGH(EN2)=NNEIGH(EN2)+1
         NNEIGH(EN3)=NNEIGH(EN3)+1
         IF((NNEIGH(EN2).GT.MNEI-1).OR.(NNEIGH(EN3).GT.MNEI-1)) GOTO 999
         NEIGH(EN2,NNEIGH(EN2))=EN3
         NEIGH(EN3,NNEIGH(EN3))=EN2
  10     CONTINUE
!
!  INSERT NODE ITSELF IN PLACE #1 and SORT other NEIGHBORS by increasing cw angle from East
!
      DO I=1,nodes
         DO J=1,NNEIGH(I)
            NEITEM(J)=NEIGH(I,J)
            DELX=X(NEITEM(J))-X(I)
            DELY=Y(NEITEM(J))-Y(I)
            DIST=SQRT(DELX*DELX+DELY*DELY)
            IF(DIST.EQ.0) GOTO 998
            IF(DELY.NE.0) THEN
              ANGLE(J)=RAD2DEG*ACOS(DELX/DIST)
              IF(DELY.GT.0) ANGLE(J)=360.-ANGLE(J)
              ENDIF
            IF(DELY.EQ.0) THEN
              IF(DELX.GT.0) ANGLE(J)=0.
              IF(DELX.LT.0) ANGLE(J)=180.
              ENDIF
            END DO
         ANGLEMORE=-1.
         DO JJ=1,NNEIGH(I)
            ANGLELOW=400.
            DO J=1,NNEIGH(I)
               IF((ANGLE(J).LT.ANGLELOW).AND.(ANGLE(J).GT.ANGLEMORE)) THEN
                  ANGLELOW=ANGLE(J)
                  JLOW=J
                  ENDIF
               END DO
            NEIGH(I,JJ+1)=NEITEM(JLOW)
            ANGLEMORE=ANGLELOW
            END DO
         NEIGH(I,1)=I
         NNEIGH(I)=NNEIGH(I)+1
         END DO

!
!   DETERMINE THE MAXIMUM AND MINIMUM NUMBER OF NEIGHBORS
!
      NEIMAX = 0
      NEIMIN = 1000
      DO 60 N=1,nodes
         IF(NNEIGH(N).LT.NEIMIN) NEIMIN=NNEIGH(N)
         IF(NNEIGH(N).GT.NEIMAX) NEIMAX=NNEIGH(N)
  60     CONTINUE
!
      RETURN

!     TERMINATE PROGRAM IF MAXIMUM NUMBER OF NEIGHBORS SET TOO SMALL

 999  CONTINUE
      IF(NSCREEN.EQ.1) WRITE(6,99311)EN1,EN2,EN3
      WRITE(16,99311)EN1,EN2,EN3
99311 FORMAT(////,1X,'!!!!!!!!!!  WARNING - FATAL ERROR !!!!!!!!!',    &
      &      //,1X,'THE DIMENSIONING PARAMETER MNEI IS TOO SMALL'      &
      &      //,1X,'NODES',3(I8,X)                                     &
      &     /,1X,'USER MUST RE-DIMENSION PROGRAM',                     &
      &     //,1X,'!!!!!! EXECUTION WILL NOW BE TERMINATED !!!!!!',//)
      STOP

 998  CONTINUE
      IF(NSCREEN.EQ.1) WRITE(6,99312) I,NEITEM(J)
      WRITE(16,99312) I,NEITEM(J)
99312 FORMAT(////,1X,'!!!!!!!!!!  WARNING - FATAL ERROR !!!!!!!!!',   &
      &      //,1X,'NODES ',I7,' AND ',I7,' HAVE THE SAME COORDINATES'&
      &     //,1X,'!!!!!! EXECUTION WILL NOW BE TERMINATED !!!!!!',//)
      STOP
      END

!-----------------------------------------------------------------------
	subroutine	tables(ic3,ine,ncon,nne,nodes,nel)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-11-18
!
!	PURPOSE:	compute tables of elements and sides
!		nne(i)   - number of elements that contain node i
!		ine(i,j) - elements that contain node i
!		ic3(i,j) - elements that share a side with element i
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	integer*4	nx(3,2)
	integer*4	ic3(MXEL,3),ncon(MXEL,3)
	integer*4	ine(MXNOD,MNEI)
	integer*4	nne(MXNOD)
	integer*4	i,j,k,nd,nodes,nel,iel,nd1,nd2
!-----------------------------------------------------------------------
!...
!...  compute the elements connected to each node
!...
        do i=1,nodes
          nne(i)=0
        enddo

        do i=1,nel
          do j=1,3
            nd=ncon(i,j)
            nne(nd)=nne(nd)+1
            ine(nd,nne(nd))=i
          enddo
        enddo
!...
!...    Determine the 3 elements attached to each element
!...
        do i=1,3
          nx(i,1)=mod(i,3)+1
          if(i.eq.1) then
            nx(i,2)=3
          else
            nx(i,2)=i-1
          endif
        enddo !i

        do i=1,nel
          do j=1,3
            ic3(i,j)=0 !index for bnd sides
            nd1=ncon(i,nx(j,1))
            nd2=ncon(i,nx(j,2))
            do k=1,nne(nd1)
              iel=ine(nd1,k)
              if(iel/=i .and.   &
        &        (ncon(iel,1)==nd2 .or. ncon(iel,2)==nd2 .or. &
        &         ncon(iel,3)==nd2)) ic3(i,j)=iel
            enddo !k
          enddo !j
        enddo !i

	return
	end

!-----------------------------------------------------------------------
	subroutine	bnodes(ncon,nneigh,ib,nodes,nel)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-11-18
!
!	PURPOSE:	Determine boundary nodes
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	integer*4	ncon(MXEL,3)
	integer*4	ine(MXNOD),nneigh(MXNOD),ib(MXNOD)
	integer*4	i,j,nodes,nel
!-----------------------------------------------------------------------
	do i = 1, nodes
	    ine(i)= 0
	end do
	do i = 1, nel
	    do j = 1, 3
		ine(ncon(i,j)) = ine(ncon(i,j))+1
	    end do
	end do
	do i = 1, nodes
	    if (ine(i)==nneigh(i)-1) then
		ib(i) = 0		!interior node
	    else
		ib(i) = 1		!boundary node
	    endif
	end do

	return
	end

!-----------------------------------------------------------------------
	subroutine	killnode(x,y,ine,ncon,neigh,nne,knode,nodes,nel, &
        &                        nei5,ib,iflag,iflagn)


!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-11-18
!
!	PURPOSE:	remove a node from a grid
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD)
	integer*4	ine(MXNOD,MNEI),neigh(MXNOD,MNEI)
	integer*4	nne(MXNOD),ib(MXNOD),iflagn(MXNOD)
	integer*4	ncon(MXEL,3)
	integer*4	i,j,k,iel,ielnew,knode,nodes,nel,newelems,nei5, &
        &               n1,n2,n3,n4,n5,iflag
!-----------------------------------------------------------------------
!  -  delete elements that contain the node
!  -  from element 1 to 1st element to kill, ncon does not change
	iel     = ine(knode,1)
	ielnew  = iel-1
!  -  from the first element to kill to the next to last
	do i = 1, nne(knode)-1
	    do j = ine(knode,i)+1,ine(knode,i+1)-1
		ielnew = ielnew+1
		iel    = iel+1
		do k = 1, 3
		    ncon(ielnew,k) = ncon(iel,k)
		end do
	    end do
	    iel = iel+1
	end do
!  -  from last element to kill to the end
	do j = ine(knode,nne(knode))+1,nel
	    ielnew = ielnew+1
	    iel    = iel+1
	    do k = 1, 3
		ncon(ielnew,k) = ncon(iel,k)
	    end do
	end do
!  -  check
	if (ielnew+nne(knode)/=nel) then
	    write(*,*)'error in killnode',ielnew,nne(knode),nel
	    stop
	endif

!  -  add elements
	newelems = nne(knode)-2+ib(knode)
	if (newelems .le. 2) then
!  -  First element
	    ncon(ielnew+1,1) = neigh(knode,4)
	    ncon(ielnew+1,2) = neigh(knode,3)
	    ncon(ielnew+1,3) = neigh(knode,2)
!  -  Second element
	    if (newelems==2) then
		ncon(ielnew+2,1) = neigh(knode,2)
		ncon(ielnew+2,2) = neigh(knode,5)
		ncon(ielnew+2,3) = neigh(knode,4)
	    endif
	elseif (newelems==3) then
	    j = 2
	    do while (neigh(knode,j)/=nei5)
		j = j+1
	    end do
	    n1 = neigh(knode,j)
	    j = j+1
	    if (j==7) j = 2
	    n2 = neigh(knode,j)
	    j = j+1
	    if (j==7) j = 2
	    n3 = neigh(knode,j)
	    j = j+1
	    if (j==7) j = 2
	    n4 = neigh(knode,j)
	    j = j+1
	    if (j==7) j = 2
	    n5 = neigh(knode,j)
	    ncon(ielnew+1,1) = n1
	    ncon(ielnew+1,2) = n2
	    ncon(ielnew+1,3) = n3
	    ncon(ielnew+2,1) = n1
	    ncon(ielnew+2,2) = n3
	    ncon(ielnew+2,3) = n4
	    ncon(ielnew+3,1) = n1
	    ncon(ielnew+3,2) = n4
	    ncon(ielnew+3,3) = n5
	else
	    write(*,*)' Too many elements to add in killnode'
	    stop
	endif
!  -  Third element
!	if (newelems==3) then
!	    ncon(ielnew+3,1) = neigh(knode,2)
!	    ncon(ielnew+3,2) = neigh(knode,6)
!	    ncon(ielnew+3,3) = neigh(knode,5)
!	endif
	nel = ielnew+newelems

!  -  Update table of elements
	do i = 1, nel
	    do j = 1, 3
		if (ncon(i,j)>=knode) ncon(i,j)=ncon(i,j)-1
	    end do
	end do
 	
!  -  delete node
	do i = knode+1,nodes
	    x(i-1) = x(i)
	    y(i-1) = y(i)
	    iflagn(i-1) = iflagn(i)
	end do
	nodes = nodes-1

	return
	end

!-----------------------------------------------------------------------
	subroutine	swaplines(ncon,iel,ielnext,nodei,nodeielnext)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-11-18
!
!	PURPOSE:	swap lines from two elements
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	integer*4	ine(MXNOD,MNEI),neigh(MXNOD,MNEI)
	integer*4	nne(MXNOD)
	integer*4	ncon(MXEL,3)
	integer*4	i,iel,nodei,ielnext,nodeielnext
!-----------------------------------------------------------------------
	if (nodei==ncon(iel,1)) then
	    ncon(ielnext,3) = ncon(iel,3)
	elseif (nodei==ncon(iel,2)) then
	    ncon(ielnext,3) = ncon(iel,1)
	    ncon(iel,2)     = ncon(iel,3)
	else
	    ncon(ielnext,3) = ncon(iel,2)
	    ncon(iel,2)     = ncon(iel,1)
	endif
	ncon(ielnext,1) = nodei
	ncon(ielnext,2) = nodeielnext
	ncon(iel,1)     = nodei
	ncon(iel,3)     = nodeielnext

	return
	end

!-----------------------------------------------------------------------
	subroutine	delelem(ielkill,ncon,nel)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-12-04
!
!	PURPOSE:	Delete two elements
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	integer*4	ncon(MXEL,3)
	integer*4	ielkill(2)
	integer*4	i,k,nel,iel,ielnew
!-----------------------------------------------------------------------
!  -  sort elements to kill
	if (ielkill(1)>ielkill(2)) then
	    i = ielkill(1)
	    ielkill(1) = ielkill(2)
	    ielkill(2) = i
	endif
!  -  from element 1 to 1st element to kill, ncon does not change
!  -  change ncon from 1st to last element to kill
	iel     = ielkill(1)
	ielnew  = iel-1
	do i = iel+1,ielkill(2)-1
	    ielnew = ielnew+1
	    iel    = iel+1
	    do k = 1, 3
		ncon(ielnew,k) = ncon(iel,k)
	    end do
	end do
!  -  change ncon from last element to kill to the end
	iel = iel+1
	do i = iel,nel
	    ielnew = ielnew+1
	    iel    = iel+1
	    do k = 1, 3
		ncon(ielnew,k) = ncon(iel,k)
	    end do
	end do
	nel = nel-2

	return
	end

!-----------------------------------------------------------------------
	subroutine	check5(node,nneigh,neigh,n5,n6,n7,nei5)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-12-08
!
!	PURPOSE:	count the number of elements per surrounding
!			node
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	integer*4	nneigh(MXNOD),neigh(MXNOD,MNEI)
	integer*4	i,n5,n6,n7,nei5,nneij,node
!-----------------------------------------------------------------------
	nei5 = 0
	n5   = 0
	n6   = 0
	n7   = 0
	do i = 2, nneigh(node)
	    nneij = nneigh(neigh(node,i))
	    if (nneij==6) then
		nei5 = neigh(node,i)
		n5   = n5+1
	    elseif (nneij==7) then
		n6   = n6+1
	    elseif (nneij==8) then
		n7   = n7+1
	    endif
	end do

	return
	end

!-----------------------------------------------------------------------
	subroutine	report(nei,icountkill,icountadd,icountswap)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2009-12-04
!
!	PURPOSE:	report actions of grid
!
!-----------------------------------------------------------------------

	integer*4	nei,icountkill,icountswap,icountadd
!-----------------------------------------------------------------------
	write(*,*)
	write(*,*)' Nodes with',nei,'elements:'
	write(*,*)'   Nodes killed: ',icountkill
	write(*,*)'   Nodes added:  ',icountadd
	write(*,*)'   Lines swapped:',icountswap
	write(*,*)' --------------------------------'
	write(*,*)

	return
	end

!-----------------------------------------------------------------------
	subroutine	calc_areas(x,y,ncon,nodes,nel)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 99-08-06
!
!	PURPOSE:	compute twice the area of the elements 
!			surrounding a node
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD)
	real*8		ae,x1,x2,x3,y1,y2,y3
	integer*4	ncon(MXEL,3)
	integer*4	nodes,nel,i,NM1,NM2,NM3,j
!-----------------------------------------------------------------------
	do i = 1, nel
	    NM1= ncon(i,1)
	    NM2= ncon(i,2)
	    NM3= ncon(i,3)
	    x1 = x(NM1)
	    x2 = x(NM2)
	    x3 = x(NM3)
	    y1 = y(NM1)
	    y2 = y(NM2)
	    y3 = y(NM3)
	    ae = x2*(y3-y1)+x1*(y2-y3)+x3*(y1-y2)
	    if (ae<0.d0) then
		j = ncon(i,2)
		ncon(i,2) = ncon(i,3)
		ncon(i,3) = j
		write(*,*)' Element corrected:',i
	    endif
	end do

	return
	end

!-----------------------------------------------------------------------
	subroutine	checks(x,y,ib,ncon,nel,nodes)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 2010-08-16
!
!	PURPOSE:	Perform checks on the grid and issue warnings,
!			if required
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD)
	real*8		ar_coor(MXEL,9),ar(MXEL)
	integer*4	ncon(MXEL,3)
	integer*4	ib(MXNOD)
	integer*4	i,j,k,nodes,nel,j_find

	real*8		x_i,y_i
!-----------------------------------------------------------------------
!  -  Compute area coordinates
	call calc_area_coords(nel,ncon,x,y,ar_coor,ar)

!  -  Check if any boundary node belongs to any elements besides
!     those defined in ncon

	do i=1, nodes
	    if(ib(i)/=0) then !boundary node
		x_i  = x(i)
		y_i  = y(i)
		do j = 1, nel
		    call in_out_soft(j,x_i,y_i,ar_coor,ar,*500,*1000)
500		    j_find  = j
		    if (ncon(j,1)/=i .and. ncon(j,2)/=i .and.   &
        &               ncon(j,3)/=i) then
			write(*,*)'WARNING! A problem with the grid ', &
        &               'around node',i,'must be corrected manually'
                        write(*,*)'It belongs to the element containing'
                        write(*,*)'nodes ',ncon(j,1),ncon(j,2),ncon(j,3)
                        write(*,*)x_i,y_i
                        write(*,*)x(ncon(j,1)),y(ncon(j,1))
                        write(*,*)x(ncon(j,2)),y(ncon(j,2))
                        write(*,*)x(ncon(j,3)),y(ncon(j,3))
		    endif
1000    	end do
	    endif
	end do

	return
	end

!-----------------------------------------------------------------------
	subroutine	calc_area_coords(n_e,el,x,y,ar_coor,ar)	
	
	include         'nicegrid2.cmn'

	integer*4	n_e
	integer*4	el(MXEL,3)	
	real*8		x(MXNOD), y(MXNOD), ar_coor(MXEL,9), ar(MXEL)
	
	integer*4	i, no1, no2, no3, aux,j
	real*8		x1, x2, x3, y1, y2, y3, area2
	
	
	
	do	i = 1, n_e
            no1	= el(i,1)
            no2	= el(i,2)
            no3	= el(i,3)		
            x1	= x(no1)
            x2	= x(no2)
            x3	= x(no3)
            y1	= y(no1)
            y2	= y(no2)
            y3	= y(no3)
            ar(i)	= 0.5d0*((x1-x3)*(y2-y3)-(x3-x2)*(y3-y1))

            if(ar(i)<0.0d0)	then
                write(*,*)'negative area at element', i,'was corrected'
                aux	= el(i,1)
                el(i,1)	= el(i,3)
                el(i,3)	= aux
                ar(i)	= -ar(i)

            elseif(ar(i)==0.0d0)	then
                write(*,*)'element', i,'has 0 area'
                stop 
            endif
            area2	= 2.0d0*ar(i)

            ar_coor(i,1)	= (x2/area2)*y3-(y2/area2)*x3
            ar_coor(i,2)	= (x3/area2)*y1-(y3/area2)*x1
            ar_coor(i,3)	= (x1/area2)*y2-(y1/area2)*x2
            ar_coor(i,4)	= (y2-y3)/area2
            ar_coor(i,5)	= (y3-y1)/area2
            ar_coor(i,6)	= (y1-y2)/area2
            ar_coor(i,7)	= (x3-x2)/area2
            ar_coor(i,8)	= (x1-x3)/area2
            ar_coor(i,9)	= (x2-x1)/area2
	end do	
		

	return
	
	end
        
!---------------------------------------------------------------------------
        subroutine global_find(x_i,y_i,n_el,ar_coor,ar,j_find)


        include         'nicegrid2.cmn'

        integer*4       no, n_el, j_find
        real*8          ar_coor(MXEL,9), ar(MXEL)
        real*8          x_i, y_i

        integer*4       i

        do i = 1, n_el
            call in_out_soft(i, x_i, y_i, ar_coor, ar,*500,*1000)
500         j_find  = i
            return
1000    end do

        write(*,*) 'Cannot find starting element for node/particle', no
        write(*,*) 'coordinates', x_i,y_i
        stop

        end

!----------------------------------------------------------------------------
	subroutine	in_out_soft(el1, xx, yy, ar_coor, ar, *, *)	
	
	include		'nicegrid2.cmn'
	
	real*8		ar_coor(MXEL,9)
	real*8		ar(MXEL)
	
	integer*4	el1
	real*8		yy, xx, l, bux1, bux3, cux1, cux2, cux3, bux2
	
	integer*4	j, flag_cor, aux
	real*8		one, zero, dux
	
	data	zero, one /-0.000001d0,1.000001d0/
	
		
	flag_cor	 = 0	 	
	
!	checks if the point is inside the original element j_i	
	do j = 1, 3
	    l = ar_coor(el1,j)+ar_coor(el1,j+3)*xx+ar_coor(el1,j+6)*yy
     
!	if any of the shape functions are > 1 or < 0, the point it is not
!	inside this element
            if((l>one) .or. (l<zero)) return 2
     		
!	if it's between 0 and zero and 1 and one, saves the 
!	area coordinate number
            if((l<0.0d0) .and. (l>=zero)) 	flag_cor = j
	end do	
	
	
!	if between 0 and zero, or 1 and one, adjusts to element boundary
	if(flag_cor>0)	then
		aux	= flag_cor+1
1999		if(aux==4)	aux = 1
		bux1	= ar_coor(el1,flag_cor)
		bux2	= ar_coor(el1,flag_cor+3)
		bux3	= ar_coor(el1,flag_cor+6)
		cux1	= ar_coor(el1,aux)
		cux2	= ar_coor(el1,aux+3)
		cux3	= ar_coor(el1,aux+6)
		
		if(bux2==0.0d0)	then
			l	= -bux1/bux3
			if(cux2==0.0d0)	then
				aux	= aux+1
				goto 1999
			endif
			xx	= ((cux2*xx +cux3*yy)-cux3*l)/cux2
			yy	= l
			return 1
		endif
			
		l	= cux2/bux2
		dux	= cux3-bux3*l

		if(dux==0.0d0) 	then
			aux	= aux + 1
			goto 1999
		endif
		yy	= cux2*xx +cux3*yy
     		yy	= (yy + bux1*l)/dux
     		
     		xx	= (- bux1 - bux3*yy)/bux2		
	endif
	
	return 1
		
	end
        
!-----------------------------------------------------------------------
	subroutine	rbpt(file,x,y,h,n)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 98-03-17
!
!	PURPOSE:	read a build points file
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD),h(MXNOD)
	integer*4	i,j,n
	character*40	file
!-----------------------------------------------------------------------
	open(1,file=file,status='old')
	read(1,*)
	read(1,*)n
	do i = 1, n
	    read(1,*)j,x(i),y(i),h(i)
	end do
	close(1)

	return
	end

!-----------------------------------------------------------------------
	subroutine	wbpt(file,x,y,h,n)

!-----------------------------------------------------------------------
!			    ,
!	AUTHOR:		Andre Fortunato - 98-03-17
!
!	PURPOSE:	writes a build points file
!
!-----------------------------------------------------------------------

	include		'nicegrid2.cmn'
	real*8		x(MXNOD),y(MXNOD)
	integer*4	h(MXNOD)
	real*4		xs,ys
	integer*4	i,n
	character*40	file
!-----------------------------------------------------------------------
	if (n>MXNOD) then
	    write(*,*)' MXNOD exceeded. Increase to ',n
	    stop
	endif
	open(1,file=file,status='unknown')
	write(1,*)' Created by program nicegrid2.f'
	write(1,*)n
	do i = 1, n
	    write(1,100)i,x(i),y(i),h(i)
	end do
	close(1)
100	format(i7,2(x,f10.2),x,i2)

	return
	end

!-----------------------------------------------------------------------
