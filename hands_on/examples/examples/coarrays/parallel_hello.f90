PROGRAM hello
  IMPLICIT none
  WRITE(*, '(''Hello from image '',i0, '' of '',i0)') this_image(), num_images()
END PROGRAM
