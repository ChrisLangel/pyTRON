!      
!     This file contains a collection of fortran subroutines that will
!     be converted into python modules

      subroutine getgriddim(gfile,mgrid,nj,nk,nl,ioerr) 
      character(180), intent(in) :: gfile
      integer, intent(in) :: mgrid
      integer, intent(out) :: nj,nk,nl
      logical, intent(out) :: ioerr  

      ioerr = .false.
      open(1,file=gfile,status='OLD',form='UNFORMATTED',err=10) 
      read(1,end=10,err=10) ngrid
      if (ngrid .eq. 1) then
         read(1,end=10,err=10) nj,nk,nl
      else
         read(1,end=10,err=10) (nj,nk,nl,n=1,mgrid)
      end if
      close(1)
      goto 20
10    continue
      close(1)
      ioerr  = .true.
20    continue
      end 

!     We are including the parameters "nx" and "ny" to represent the
!     number of points in the horizontal and vertical directions
!     respectively. "cp" represents the constant plane L=1,K=2,J=3.
!     "cpn" is the point in that plane held constant

      subroutine getgridcords(gfile,mgrid,nj,nk,nl,
     &                        cp,cpn,nx,ny,xt,yt,ioerr)
      character(180), intent(in) :: gfile
      integer,intent(in) :: mgrid,nj,nk,nl,nx,ny,cp,cpn
      real(kind=8),dimension(nx,ny),intent(out) :: xt,yt 
      logical, intent(out) :: ioerr
      real(kind=8),dimension(nj,nk,nl) :: x,y,z
      integer :: i,jj,kk,ll
      
      ioerr = .false.
      open(1,file=gfile, status='OLD', form='UNFORMATTED',err=10) 
      read(1,end=10,err=10)
      read(1,end=10,err=10)    
      do i=1,mgrid-1
         read(1,err=10,end=10) 
      end do 

      read(1,err=10,end=10) (((x(jj,kk,ll),jj=1,nj),kk=1,nk),ll=1,nl),
     &        (((y(jj,kk,ll),jj=1,nj),kk=1,nk),ll=1,nl),
     &        (((z(jj,kk,ll),jj=1,nj),kk=1,nk),ll=1,nl)

      close(1)

!     Really only care about one plane and it is much more efficient to
!     do this here rather than back in the GUI

      if (cp .eq. 1) then
         xt = x(:,:,cpn)
         yt = z(:,:,cpn)
      else if (cp .eq. 2) then
         xt = x(:,cpn,:)
         yt = y(:,cpn,:)
      else 
         xt = y(cpn,:,:)
         yt = z(cpn,:,:)
      end if 
      goto 20
 10   continue
      close(1)  
      ioerr = .true. 
 20   continue   
      end


      subroutine readheader(qfile,mgrid,ngrid,nj,nk,nl,nq,nqc, 
     &                      refmach,alfa,rey,time,gaminf,tinf,
     &                      beta,fsmach,ioerr) 

      character(180),intent(in) :: qfile
      integer,intent(in) :: mgrid
      integer, intent(out) :: ngrid,nj,nl,nk,nq,nqc 
      real(kind=8),intent(out) :: refmach,alfa,rey,time,gaminf,tinf,
     &                             beta,fsmach
      logical, intent(out) :: ioerr 
      integer :: njd,nkd,nld,n,igam
      real(kind=8) :: htinf,ht1,ht2,tvref,dtref
      real(kind=8),dimension(:),allocatable :: rgas

      ioerr = .false.
      open(1,file=qfile,status='OLD',form='UNFORMATTED') 
      read(1,err=30,end=10) nj,nk,nl
      rewind(1)
      ngrid = 1 
      goto 50    
30    rewind(1) 
      read(1,err=10,end=10) ngrid  
50    if (ngrid .eq. 1) then
         read(1,err=10,end=10) nj,nk,nl,nq,nqc 
      else
         read(1,err=10,end=10) (nj,nk,nl,n=1,mgrid),
     &      (njd,nkd,nld,n=1,ngrid-mgrid),nq,nqc
      end if 
      do n = 1,mgrid-1
         read(1,err=10,end=10)
         read(1,err=10,end=10)
      end do 
      allocate( rgas(max(2,nqc)) )
      read(1,err=10,end=10) refmach,alfa,rey,time,gaminf,beta,tinf,
     &        igam,htinf,ht1,ht2,rgas(1:max(2,nqc)),
     &        fsmach,tvref,dtref

      close(1)
      goto 20
10    continue
      close(1)
      ioerr = .true. 
20    continue
      end


      subroutine readq(qfile,ngrid,mgrid,nj,nk,nl,nq,cq,
     &                 nx,ny,cp,cpn,gaminf,mach,qt,ioerr)
      
      character(180),intent(in) :: qfile
      integer,intent(in) :: mgrid,ngrid,nj,nk,nl,nq,cq,
     &                      nx,ny,cp,cpn
      real(kind=8), intent(in) :: gaminf,mach
      real(kind=8),dimension(nx,ny),intent(out) :: qt
      logical, intent(out) :: ioerr
      integer :: n,j,k,l,njt,nkt,nlt     
      real(kind=8) :: u,v,w,vsq,e,pinf,qinf,rqinf,p,rho
      real(kind=8),dimension(nj,nk,nl) :: q  
      real(kind=8),dimension(nj,nk,nl,nq) :: tempq 
      


      ioerr = .false.
      open(1,file=qfile,status='OLD',form='UNFORMATTED') 


      read(1,err=30) njt,nkt,nlt
      rewind(1) 
      goto 50
 30   rewind(1)  
      read(1,err=10,end=10)  
 50   continue
!     Skip all the header info       
      
      read(1,err=10,end=10)
      !else 
      !   read(1,err=10,end=10) 
      !   read(1,err=10,end=10)
      !end if
      
      do n = 1,mgrid-1
         read(1,err=10,end=10)
         read(1,err=10,end=10) 
      end do 

!     Actually read the q array for the desired grid 
      read(1,err=10,end=10) ! Skip header 
      read(1,err=10,end=10) tempq 
      if (cq .lt. 15) then 
         q = tempq(:,:,:,cq)
      else if (cq .eq. 15) then
         do j = 1,nj
            do k = 1,nk
               do l = 1,nl
                  pinf  = 1./gaminf 
                  qinf  = 0.5*mach**2
                  rqinf = 1./qinf 
                  rho   = 1./tempq(j,k,l,1)
                  u     = tempq(j,k,l,2)*rho
                  v     = tempq(j,k,l,3)*rho
                  w     = tempq(j,k,l,4)*rho
                  e     = tempq(j,k,l,5)
                  vsq = u**2 + v**2 + w**2
                  p   = (gaminf-1.0)*(e-0.5*tempq(j,k,l,1)*vsq)
                  q(j,k,l) = (p - pinf)*rqinf
               end do 
            end do 
         end do 
      else if (cq .eq. 16) then
         do j = 1,nj
            do k = 1,nk
               do l = 1,nl
                  u = tempq(j,k,l,2)
                  v = tempq(j,k,l,3)
                  w = tempq(j,k,l,4)
                  vsq = u**2 + v**2 + w**2
                  q(j,k,l) = sqrt(vsq)
               end do 
            end do 
         end do 
      end if   
  
      if (cp .eq. 1) then
         qt = q(:,:,cpn) 
      else if (cp .eq. 2) then
         qt = q(:,cpn,:)  
      else 
	 qt = q(cpn,:,:)
      end if                   
 
      close(1)
      goto 20
 10   continue 
      close(1)
      ioerr = .true.
 20   continue
      end 
!
      subroutine readdiffhead(qfile,nj,nk,nq,ioerr) 

      character(180),intent(in) :: qfile
      integer, intent(out) :: nj,nk,nq
      logical, intent(out) :: ioerr
      
      ioerr = .false.
      open(1,file=qfile,status='OLD',form='UNFORMATTED') 
      read(1,err=10,end=10)
      read(1,err=10,end=10)
      read(1,err=10,end=10) nj,nk,nq
      close(1)
      goto 20
 10   continue
      close(1)
      ioerr = .true.
 20   continue
      end


      subroutine readdiffq(qfile,nx,ny,nq,cq,qout,ioerr)
      
      character(180),intent(in) :: qfile
      integer,intent(in) :: nx,ny,nq,cq
      real(kind=8),dimension(nx,ny),intent(out) :: qout
      logical, intent(out) :: ioerr
      real(kind=8),dimension(nx,ny,nq) :: qt

      ioerr = .false.
      open(1,file=qfile,status='OLD',form='UNFORMATTED') 
      read(1,err=10,end=10) ! skip header 
      read(1,err=10,end=10) ! skip header 
      read(1,err=10,end=10) ! skip header 
      read(1,err=10,end=10) qt
      qout = qt(:,:,cq) 
      close(1)
      goto 20
 10   continue 
      close(1) 
      ioerr = .true.
 20   continue 
      end




      subroutine stretch_norm(x,y,stretch,nx,ny,xstr,ystr)
      integer,intent(in) :: nx,ny,stretch
      real(kind=8),dimension(nx,ny),intent(in) :: x,y 
      real(kind=8),dimension(nx,ny),intent(out) :: xstr,ystr
      real(kind=8) :: xa,ya,xb,yb,vax,vay
      integer :: j,k

      do k = 1,ny
         do j =1,nx
            xa = x(j,1)
            ya = y(j,1)
            xb = x(j,k)
            yb = y(j,k) 
            vax = xb - xa
            vay = yb - ya
            xstr(j,k) = xb + stretch*vax
            ystr(j,k) = yb + stretch*vay 
         end do
      end do 
      end


      subroutine readqvel(qfile,ngrid,mgrid,nj,nk,nl,nq,
     &                 nx,ny,cp,cpn,u,v,ioerr)
      
      character(180),intent(in) :: qfile
      integer,intent(in) :: mgrid,ngrid,nj,nk,nl,nq,
     &                      nx,ny,cp,cpn
      real(kind=8),dimension(nx,ny),intent(out) :: u,v
      logical, intent(out) :: ioerr 
      real(kind=8),dimension(nj,nk,nl) :: qu,qv
      integer :: n,njt,nkt,nlt      
      real(kind=8),dimension(nj,nk,nl,nq) :: tempq 

      ioerr = .false.
      open(1,file=qfile,status='OLD',form='UNFORMATTED') 


      read(1,err=30) njt,nkt,nlt
      rewind(1)
      goto 50
30    rewind(1) 
      read(1,err=10,end=10) 
50    continue

!     Skip all the header info  
      read(1,err=10,end=10)      
      
      do n = 1,mgrid-1
         read(1,err=10,end=10)
         read(1,err=10,end=10) 
      end do 

!     Actually read the q array for the desired grid 
      read(1,err=10,end=10) ! Skip header 
      read(1,err=10,end=10) tempq 
      qu = tempq(:,:,:,2)
      qv = tempq(:,:,:,4) 
! 
      if (cp .eq. 1) then
         u = qu(:,:,cpn) 
         v = qv(:,:,cpn) 
      else if (cp .eq. 2) then
         u = qu(:,cpn,:) 
         v = qv(:,cpn,:) 
      else 
         u = qu(cpn,:,:)
         v = qv(cpn,:,:)
      end if 

      close(1)
      goto 20
10    continue
      close(1)
      ioerr = .true.
20    continue  
      end 
    

 




