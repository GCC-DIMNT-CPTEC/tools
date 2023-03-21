! A derived type for storing data.
module moddata
  implicit none

  private
  public :: data_t

  ! Data is stored in data_t
  type :: data_t
     integer :: index_value
  end type data_t

end module moddata