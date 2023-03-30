! A simple generic linked list test program
program Unity_tests

  use Moddata

  implicit none

  integer :: test_errors_sum
  
  print*, ""
  print*, ">>>>> Running Unity Tests"

  test_errors_sum = 0
  test_errors_sum = test_errors_sum + test_list_init()
  test_errors_sum = test_errors_sum + test_list_inserts()
  test_errors_sum = test_errors_sum + test_list_removes()
  
  test_errors_sum = test_errors_sum + test_vector_init()
  test_errors_sum = test_errors_sum + test_vector_inserts()
  test_errors_sum = test_errors_sum + test_vector_removes()
  
  print *, "" 
  if (test_errors_sum == 0) then
    print*, ">>>>> ALL TESTS OK !"
    call exit(0)
  else
    print*, ">>>>> TESTS FAILED. TOTAL TESTS FAILED = ", test_errors_sum
    call exit(-1)
  endif


  contains 

    ! List Tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    integer function test_list_init() result(test_error)
      use Modlist
      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), allocatable :: dat_to_insert
      type(data_t) :: dat_test

      print*, ""
      print*, ">>>>> Running Test List Initilize"
      test_error = 0

      allocate(dat_to_insert)
      dat_to_insert%index_value = 1

      call init(ll, dat_to_insert)
      print *, 'Initializing list with data:', dat_to_insert

      print *, 'Testing head node'      
      dat_test = get(ll)
      if (dat_test%index_value /= 1) then
        print *, 'Head node data should be: 1 but was', dat_test%index_value
        test_error = 1
      endif
      
      call free_memory(ll)
      return

    end function test_list_init


    integer function test_list_inserts() result(test_error)
      use Modlist
      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), allocatable :: dat_x
      type(data_t) :: dat_test

      print*, ""
      print*, ">>>>> Running Test List Insert second element"
      test_error = 0

      allocate(dat_x)
      dat_x%index_value = 1
      call init(ll, dat_x)
      print *, 'Initializing list with data:', dat_x%index_value
      deallocate(dat_x)

      allocate(dat_x)
      dat_x%index_value = 2
      call insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%index_value
      deallocate(dat_x)

      print *, 'Testing head node'      
      dat_test = get(ll)
      if (dat_test%index_value /= 2) then
        print *, '!!!!! TEST FAILED !!!!! Head node data should be: 2 but was', dat_test%index_value
        call free_memory(ll)
        test_error = 1
        return
      endif
      
      print *, 'Testing second node'
      dat_test = get(next(ll))
      if (dat_test%index_value /= 1) then
        print *, '!!!!! TEST FAILED !!!!! Second node data should be: 1 but was', dat_test%index_value
        call free_memory(ll)
        test_error = 1
        return
      endif

      ! Free the list
      call free_memory(ll)

    end function test_list_inserts


    integer function test_list_removes() result(test_error)
      use Modlist
      implicit none 

      type(list_t), pointer :: ll, node_curr  => null()
      type(data_t) :: dat_x
      type(data_t) :: dat_test
      integer :: test_value
      logical :: is_removed 

      print*, ""
      print*, ">>>>> Running Test List removes elements"
      test_error = 0

      dat_x%index_value = 10
      call init(ll, dat_x)
      print *, 'Initializing list with data:', dat_x%index_value
      dat_x%index_value = 20
      call insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%index_value
      dat_x%index_value = 30
      call insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%index_value
      dat_x%index_value = 40
      call insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%index_value

      print *, 'removes last element'
      dat_x%index_value = 10
      is_removed = remove(ll, dat_x)

      print *, 'Testing nodes'      
      node_curr => ll
      test_value = 40
      do
        dat_test = get(node_curr)
        print *, 'Checking node ', dat_test%index_value
        if (dat_test%index_value /= test_value) then
          print *, '!!!!! TEST FAILED !!!!! Node data should be: ',test_value,' but was', dat_test%index_value
          call free_memory(ll)
          test_error = 1
          return
        endif
        if (.not. associated(next(node_curr))) exit
        node_curr => next(node_curr)
        test_value = test_value - 10
      enddo

      if(test_value == 10 ) then
        print *, '!!!!! TEST FAILED !!!!! last element not removed'
        call free_memory(ll)
        test_error = 1
        return
      endif
      
      print *, 'removes second element'
      dat_x%index_value = 30
      is_removed = remove(ll, dat_x)
      node_curr => ll
      dat_test = get(node_curr)
      print *, 'Checking node ', dat_test%index_value
      test_value = 40
      if (dat_test%index_value /= test_value) then
        print *, '!!!!! TEST FAILED !!!!! Node data should be: ',test_value,' but was', dat_test%index_value
        call free_memory(ll)
        test_error = 1
        return
      endif
      node_curr => next(node_curr)
      dat_test = get(node_curr)
      print *, 'Checking node ', dat_test%index_value
      test_value = 20
      if (dat_test%index_value /= test_value) then
        print *, '!!!!! TEST FAILED !!!!! Node data should be: ',test_value,' but was', dat_test%index_value
        call free_memory(ll)
        test_error = 1
        return
      endif

      print *, 'removes first element'
      dat_x%index_value = 40
      is_removed = remove(ll, dat_x)
      node_curr => ll
      dat_test = get(node_curr)
      print *, 'Checking node ', dat_test%index_value
      test_value = 20
      if (dat_test%index_value /= test_value) then
        print *, '!!!!! TEST FAILED !!!!! Node data should be: ',test_value,' but was', dat_test%index_value
        call free_memory(ll)
        test_error = 1
        return
      endif

      print *, 'removes the only one element '
      dat_x%index_value = 20
      is_removed = remove(ll, dat_x)
      if (associated(ll)) then
        print *, '!!!!! TEST FAILED !!!!! List should not contains elements'
        call free_memory(ll)
        test_error = 1
        return
      endif

    end function test_list_removes


    ! Vector Tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    integer function test_vector_init() result(test_error)
      use ModVector
      implicit none 

      integer, parameter :: p_vector_size = 1000000

      print*, ""
      print*, ">>>>> Running Test Vector Initilize SINGLETON INTERFACE"
      test_error = 0

      call init_instance()
      print *, 'Testing get_size = 0'
      if(get_size() /= 0) then
        print *, '!!!!! TEST FAILED !!!!! Size of vector should be: 0 but was', get_size()
        test_error = 1
        call free_memory()
        return
      endif

      print *, 'Initializing Vector with size = ', p_vector_size
      call init(p_vector_size)

      print *, 'Testing get_size = ', p_vector_size
      if(get_size() /= p_vector_size) then
        print *, '!!!!! TEST FAILED !!!!! Size of vector should be: ', p_vector_size,' but was', get_size()
        test_error = 1
        call free_memory()
        return
      endif

      print *, 'Testing num_elements = 0'
      if(get_num_elements() /= 0) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: 0 but was', get_num_elements()
        test_error = 1
        call free_memory()
        return
      endif

      call free_memory()

    end function test_vector_init


    integer function test_vector_inserts() result(test_error)
      use ModVector
      implicit none 

      type(data_t) :: dat_to_insert
      type(data_t) :: dat_test
      integer, parameter :: p_vector_size = 1000000
      
      print*, ""
      print*, ">>>>> Running Test Vector Insert elements"
      test_error = 0

      call init_instance()
      print *, 'Initializing Vector with size = ', p_vector_size
      call init(p_vector_size)

      dat_to_insert%index_value = 10
      print *, 'Inserting vector element with: ', dat_to_insert%index_value
      call insert(dat_to_insert)

      print *, 'Testing num_elements = 1'
      if(get_num_elements() /= 1) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: 1 but was', get_num_elements()
        test_error = 1
        call free_memory()
        return
      endif

      print *, 'Testing first element'
      dat_test = get(get_num_elements())
      if(dat_test%index_value /= 10) then
        print *, '!!!!! TEST FAILED !!!!! First element should be: 10 but was', dat_test%index_value
        test_error = 1
        call free_memory()
        return
      endif

      dat_to_insert%index_value = 20
      print *, 'Inserting vector element with: ', dat_to_insert%index_value
      call insert(dat_to_insert)

      print *, 'Testing num_elements = 2'
      if(get_num_elements() /= 2) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: 2 but was', get_num_elements()
        test_error = 1
        call free_memory()
        return
      endif

      print *, 'Testing second element'
      dat_test = get(get_num_elements())
      if(dat_test%index_value /= 20) then
        print *, '!!!!! TEST FAILED !!!!! First element should be: 20 but was', dat_test%index_value
        test_error = 1
        call free_memory()
        return
      endif

      call free_memory()

    end function test_vector_inserts


    integer function test_vector_removes() result(test_error)
      use ModVector
      implicit none 

      type(data_t) :: dat_to_insert, dat_to_remove
      type(data_t) :: dat_test
      integer, parameter :: p_vector_size = 1000000
      integer :: num_elements_test, index_element, test_value
      logical :: dummy
      
      print*, ""
      print*, ">>>>> Running Test Vector Removes elements"
      test_error = 0

      call init_instance()
      print *, 'Initializing Vector with size = ', p_vector_size
      call init(p_vector_size)

      dat_to_insert%index_value = 10
      print *, 'Insertting elements in vector:' 
      call insert(dat_to_insert)
      dat_to_insert%index_value = 20
      call insert(dat_to_insert)
      dat_to_insert%index_value = 30
      call insert(dat_to_insert)
      dat_to_insert%index_value = 40
      call insert(dat_to_insert)
      call print_all()

      print *, 'Testing remove last '
      dat_to_remove%index_value = 40
      dummy = remove(dat_to_remove)
      num_elements_test = 3
      print *, 'Testing num_elements = ', num_elements_test
      if(get_num_elements() /= num_elements_test) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: ', num_elements_test, ' but was ', get_num_elements()
        test_error = 1
        call free_memory()
        return
      endif
      print *, 'Testing elements'
      call print_all()
      test_value = 10
      do index_element = 1, 3
        dat_test = get(index_element)
        if(dat_test%index_value /= test_value) then
          print *, '!!!!! TEST FAILED !!!!! element index', index_element, ' should be: ', test_value, ' but was', dat_test%index_value
          test_error = 1
          call free_memory()
          return
        endif
        test_value = test_value + 10
      enddo

      print *, 'Testing remove index 2'
      dat_to_remove%index_value = 20
      dummy = remove(dat_to_remove)
      num_elements_test = 2
      print *, 'Testing num_elements = ', num_elements_test
      if(get_num_elements() /= num_elements_test) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: ', num_elements_test, ' but was ', get_num_elements()
        test_error = 1
        call free_memory()
        return
      endif
      print *, 'Testing elements'
      call print_all()
      test_value = 10
      dat_test = get(1)
      if(dat_test%index_value /= test_value) then
        print *, '!!!!! TEST FAILED !!!!! First element should be: ', test_value, ' but was', dat_test%index_value
        test_error = 1
        call free_memory()
        return
      endif
      test_value = 30
      dat_test = get(2)
      if(dat_test%index_value /= test_value) then
        print *, '!!!!! TEST FAILED !!!!! First element should be: ', test_value, ' but was', dat_test%index_value
        test_error = 1
        call free_memory()
        return
      endif

      print *, 'Testing remove index 1'
      dat_to_remove%index_value = 10
      dummy = remove(dat_to_remove)
      num_elements_test = 1
      print *, 'Testing num_elements = ', num_elements_test
      if(get_num_elements() /= num_elements_test) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: ', num_elements_test, ' but was ', get_num_elements()
        test_error = 1
        call free_memory()
        return
      endif
      print *, 'Testing elements'
      call print_all()
      test_value = 30
      dat_test = get(1)
      if(dat_test%index_value /= test_value) then
        print *, '!!!!! TEST FAILED !!!!! First element should be: ', test_value, ' but was', dat_test%index_value
        test_error = 1
        call free_memory()
        return
      endif

      call free_memory()

    end function test_vector_removes


end program Unity_tests