      subroutine getgriddimen(gfile,mgrid)
      character(80), intent(in) :: gfile
      integer, intent(in) :: mgrid
      real(kind=8),dimension(:,:,:),allocatable,intent(out):: x,y,z
      integer :: i,ngrid,nj,nk,nl,n,jj,kk,ll
      

      open(1,file=gfile, status='OLD', form='UNFORMATTED') 
      read(1) ngrid
      read(1) (nj,nk,nl,n=1,mgrid)
     
      print *, nj,nk,nl 
      
      allocate( x(nj,nk,nl),y(nj,nk,nl),z(nj,nk,nl) )
      do i=1,mgrid-1
         read(1) 
      end do 
      


      read(1) (((x(jj,kk,ll),jj=1,nj),kk=1,nk),ll=1,nl),
     &        (((y(jj,kk,ll),jj=1,nj),kk=1,nk),ll=1,nl),
     &        (((z(jj,kk,ll),jj=1,nj),kk=1,nk),ll=1,nl)


      end
