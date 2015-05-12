!      
!     This file contains a collection of fortran subroutines that will
!     be converted into python modules

      subroutine getgriddim(gfile,mgrid,nj,nk,nl) 
      character(180), intent(in) :: gfile
      integer, intent(in) :: mgrid
      integer, intent(out) :: nj,nk,nl
      
      open(1,file=gfile,status='OLD', form='UNFORMATTED') 
      read(1) ngrid
      if (ngrid .eq. 1) then
         read(1) nj,nk,nl
      else
         read(1) (nj,nk,nl,n=1,mgrid)
      end if
      print *, nj,nk,nl
      close(1)
      end 

!     We are including the parameters "nx" and "ny" to represent the
!     number of points in the horizontal and vertical directions
!     respectively. "cp" represents the constant plane L=1,K=2,J=3.
!     "cpn" is the point in that plane held constant

      subroutine getgridcords(gfile,mgrid,nj,nk,nl,
     &                        cp,cpn,nx,ny,xt,yt)
      character(180), intent(in) :: gfile
      integer,intent(in) :: mgrid,nj,nk,nl,nx,ny,cp,cpn
      real(kind=8),dimension(nx,ny),intent(out) :: xt,yt 
      real(kind=8),dimension(nj,nk,nl) :: x,y,z
      integer :: i,jj,kk,ll
      
      open(1,file=gfile, status='OLD', form='UNFORMATTED') 
      read(1)
      read(1)    
      do i=1,mgrid-1
         read(1) 
      end do 

      read(1) (((x(jj,kk,ll),jj=1,nj),kk=1,nk),ll=1,nl),
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
      end


      subroutine readheader(qfile,mgrid,ngrid,nj,nk,nl,nq,nqc, 
     &                      refmach,alfa,rey,time,gaminf,tinf,
     &                      beta,fsmach) 

      character(180),intent(in) :: qfile
      integer,intent(in) :: mgrid
      integer, intent(out) :: ngrid,nj,nl,nk,nq,nqc 
      real(kind=8),intent(out) :: refmach,alfa,rey,time,gaminf,tinf,
     &                             beta,fsmach 
      integer :: njd,nkd,nld,n,igam
      real(kind=8) :: htinf,ht1,ht2,tvref,dtref
      real(kind=8),dimension(:),allocatable :: rgas

      open(1,file=qfile,status='OLD',form='UNFORMATTED') 
      read(1,err=30) nj,nk,nl
      rewind(1)
      ngrid = 1 
      goto 50    
30    rewind(1) 
      read(1) ngrid  
50    if (ngrid .eq. 1) then
         read(1) nj,nk,nl,nq,nqc 
      else
         read(1) (nj,nk,nl,n=1,mgrid),
     &      (njd,nkd,nld,n=1,ngrid-mgrid),nq,nqc
      end if 
      do n = 1,mgrid-1
         read(1)
         read(1)
      end do 
      allocate( rgas(max(2,nqc)) )
      read(1) refmach,alfa,rey,time,gaminf,beta,tinf,
     &        igam,htinf,ht1,ht2,rgas(1:max(2,nqc)),
     &        fsmach,tvref,dtref

      close(1)
      end


      subroutine readq(qfile,ngrid,mgrid,nj,nk,nl,nq,cq,
     &                 nx,ny,cp,cpn,qt)
      
      character(180),intent(in) :: qfile
      integer,intent(in) :: mgrid,ngrid,nj,nk,nl,nq,cq,
     &                      nx,ny,cp,cpn
      real(kind=8),dimension(nx,ny),intent(out) :: qt
      real(kind=8),dimension(nj,nk,nl) :: q
      integer :: n      
      real(kind=8),dimension(nj,nk,nl,nq) :: tempq 

      open(1,file=qfile,status='OLD',form='UNFORMATTED') 

!     Skip all the header info       
      if (ngrid .eq. 1) then
         read(1)
      else 
         read(1) 
         read(1)
      end if
      
      do n = 1,mgrid-1
         read(1)
         read(1) 
      end do 

!     Actually read the q array for the desired grid 
      read(1) ! Skip header 
      read(1) tempq 
      q = tempq(:,:,:,cq)
! 
      if (cp .eq. 1) then
         qt = q(:,:,cpn) 
      else if (cp .eq. 2) then
         qt = q(:,cpn,:) 
      else 
         qt = q(cpn,:,:)
      end if 

      close(1)
      end 
!
!      
      subroutine readdiffhead(qfile,nj,nk) 

      character(180),intent(in) :: qfile
      integer, intent(out) :: nj,nk

      open(1,file=qfile,status='OLD',form='UNFORMATTED') 
      read(1) nj,nk
      close(1)

      end


      subroutine readdiffq(qfile,nx,ny,cq,qout)
      
      character(180),intent(in) :: qfile
      integer,intent(in) :: nx,ny,cq
      real(kind=8),dimension(nx,ny),intent(out) :: qout
      real(kind=8),dimension(nx,ny,3) :: qt

      open(1,file=qfile,status='OLD',form='UNFORMATTED') 
      read(1) ! skip header

!     Actually read the q array for the desired grid 
      read(1) qt
      qout = qt(:,:,cq) 
      close(1)
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
     &                 nx,ny,cp,cpn,u,v)
      
      character(180),intent(in) :: qfile
      integer,intent(in) :: mgrid,ngrid,nj,nk,nl,nq,
     &                      nx,ny,cp,cpn
      real(kind=8),dimension(nx,ny),intent(out) :: u,v
      real(kind=8),dimension(nj,nk,nl) :: qu,qv
      integer :: n      
      real(kind=8),dimension(nj,nk,nl,nq) :: tempq 

      open(1,file=qfile,status='OLD',form='UNFORMATTED') 

!     Skip all the header info       
      if (ngrid .eq. 1) then
         read(1)
      else 
         read(1) 
         read(1)
      end if
      
      do n = 1,mgrid-1
         read(1)
         read(1) 
      end do 

!     Actually read the q array for the desired grid 
      read(1) ! Skip header 
      read(1) tempq 
      qu = tempq(:,:,:,2)
      qv = tempq(:,:,:,3) 
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
      end 
    

 




