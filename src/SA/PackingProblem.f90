PROGRAM Packing2D
  USE mtdefs
  USE mtmod
  USE SAalgo
  IMPLICIT NONE
  REAL(rk_mtmod) :: k
  TYPE(rect), DIMENSION(10) :: table
  
  CALL sgrnd(getseed(info=1))
  
  table = initialize(10)
  CALL draw(table, 'initial.dat',10)
  CALL draw(SA(table), 'result.dat',11)

CONTAINS
    
    
    
    !Draws rectangles from input array l
    !and saves it in "image.dat" file
    SUBROUTINE draw(l, name, num)
    IMPLICIT NONE
    TYPE(rect), DIMENSION(:), INTENT(IN) :: l
    CHARACTER(*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: num
    INTEGER :: ierror, i, n
    REAL :: x, y, w, h

    n = SIZE(l)
    
    OPEN(num, FILE=name, ACTION='write', STATUS='replace', IOSTAT=ierror)
    PRINT*, 'IOSTAT: ', ierror
    DO i=1, n
        w = l(i)%w
        h = l(i)%h
        x = l(i)%r(1)
        y = l(i)%r(2)
        WRITE(num, "('move ', 2F10.3)")   x-w/2, y+h/2
        WRITE(num, "(2F10.3)") x+w/2, y+h/2, x+w/2, y-h/2, x-w/2, y-h/2, x-w/2, y+h/2
    END DO
    CLOSE(num)
    END SUBROUTINE draw
    
    

END PROGRAM Packing2D

