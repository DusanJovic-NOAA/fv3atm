#define ESMF_ERR(rc) \
  if (rc /= 0) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
  if (rc /= 0) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#define NC_ERR(status) \
  if (status /= nf90_noerr) write(0,'(A,A,I0,A,A)') __FILE__,':',__LINE__, ' NetCDF error: ',trim(nf90_strerror(status)); \
  if (status /= nf90_noerr) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#define ASSERT(a) \
  if ((a) .neqv. .true. ) write(0,'(A,A,I0,A)') __FILE__,':',__LINE__, ' assertion failed'; \
  if ((a) .neqv. .true. ) stop 1

module mpas_esmf_mesh

   implicit none

   private

   public :: create_mpas_esmf_mesh_from_file

contains

   subroutine create_mpas_esmf_mesh_from_file(mesh, nprocs, localpet, fname, fname_part, rc)

      use netcdf
      use esmf

      implicit none

      type(ESMF_Mesh), intent(out) :: mesh
      integer, intent(in) :: nprocs
      integer, intent(in) :: localpet
      character(len=*), intent(in) :: fname, fname_part
      integer, intent(out) :: rc

      integer :: i, j, iloc, ncerr, mv, k
      integer :: ncid, dimid, varid
      integer :: nCells, nEdges, nVertices, maxEdges, vertexDegree
      integer :: nCells_on_pet, nVertex_on_pet

      real, dimension(:), allocatable :: lonCell, latCell, latVertex, lonVertex
      integer, dimension(:), allocatable :: nEdgesOnCell, locNodeID
      integer, dimension(:,:), allocatable :: verticesOnCell
      integer, dimension(:,:), allocatable :: cellsOnVertex

      integer, dimension(:), allocatable :: part_ids

      integer :: numNodes
      integer, dimension(:), allocatable :: nodeIds, nodeOwners
      real(ESMF_KIND_R8), dimension(:), allocatable :: nodeCoords

      integer :: numElems, numElemsConn, ielemConn
      integer, dimension(:), allocatable :: elemIds, elemTypes, elemConn
      real(ESMF_KIND_R8), dimension(:), allocatable :: elemCoords


      ncerr = nf90_open(trim(fname),nf90_nowrite,ncid); NC_ERR(ncerr)

      ncerr = nf90_inq_dimid(ncid, 'nCells', dimid); NC_ERR(ncerr)
      ncerr = nf90_inquire_dimension(ncid, dimid, len=nCells); NC_ERR(ncerr)
      ncerr = nf90_inq_dimid(ncid, 'nEdges', dimid); NC_ERR(ncerr)
      ncerr = nf90_inquire_dimension(ncid, dimid, len=nEdges); NC_ERR(ncerr)
      ncerr = nf90_inq_dimid(ncid, 'nVertices', dimid); NC_ERR(ncerr)
      ncerr = nf90_inquire_dimension(ncid, dimid, len=nVertices); NC_ERR(ncerr)

      ncerr = nf90_inq_dimid(ncid, 'maxEdges', dimid); NC_ERR(ncerr)
      ncerr = nf90_inquire_dimension(ncid, dimid, len=maxEdges); NC_ERR(ncerr)
      ncerr = nf90_inq_dimid(ncid, 'vertexDegree', dimid); NC_ERR(ncerr)
      ncerr = nf90_inquire_dimension(ncid, dimid, len=vertexDegree); NC_ERR(ncerr)

      ! print *, 'nCells = ', nCells
      ! print *, 'nEdges = ', nEdges
      ! print *, 'nVertices = ', nVertices
      ! print *, 'maxEdges = ', maxEdges
      ! print *, 'vertexDegree = ', vertexDegree

      allocate(latCell(nCells))
      allocate(lonCell(nCells))

      allocate(latVertex(nVertices))
      allocate(lonVertex(nVertices))

      allocate(nEdgesOnCell(nCells))
      allocate(verticesOnCell(maxEdges, nCells))
      allocate(cellsOnVertex(vertexDegree, nVertices))

      ncerr = nf90_inq_varid(ncid, 'latCell', varid); NC_ERR(ncerr)
      ncerr = nf90_get_var(ncid, varid, latCell); NC_ERR(ncerr)
      ncerr = nf90_inq_varid(ncid, 'lonCell', varid); NC_ERR(ncerr)
      ncerr = nf90_get_var(ncid, varid, lonCell); NC_ERR(ncerr)
      ncerr = nf90_inq_varid(ncid, 'nEdgesOnCell', varid); NC_ERR(ncerr)
      ncerr = nf90_get_var(ncid, varid, nEdgesOnCell); NC_ERR(ncerr)
      ncerr = nf90_inq_varid(ncid, 'latVertex', varid); NC_ERR(ncerr)
      ncerr = nf90_get_var(ncid, varid, latVertex); NC_ERR(ncerr)
      ncerr = nf90_inq_varid(ncid, 'lonVertex', varid); NC_ERR(ncerr)
      ncerr = nf90_get_var(ncid, varid, lonVertex); NC_ERR(ncerr)
      ncerr = nf90_inq_varid(ncid, 'verticesOnCell', varid); NC_ERR(ncerr)
      ncerr = nf90_get_var(ncid, varid, verticesOnCell); NC_ERR(ncerr)
      ncerr = nf90_inq_varid(ncid, 'cellsOnVertex', varid); NC_ERR(ncerr)
      ncerr = nf90_get_var(ncid, varid, cellsOnVertex); NC_ERR(ncerr)

      allocate(part_ids(nCells))

      if (nprocs>1) then
         open(unit=10,file=trim(fname_part), status='old', action='read')
         do i =1,nCells
            read(10,*) part_ids(i)
         end do
         close(10)
      else
         part_ids = 0
      end if

      ! define elemIds, elemCoords, elemTypes, and elemConn
      nCells_on_pet = 0
      do i =1,nCells
         if (localpet == part_ids(i)) then
            nCells_on_pet = nCells_on_pet + 1
         end if
      end do

      numElems = nCells_on_pet
      allocate(elemIds(numElems))
      allocate(elemCoords(2*numElems))
      allocate(elemTypes(numElems))
      numElemsConn = 0
      iloc = 0
      do i =1,nCells
         if (localpet == part_ids(i)) then
            iloc = iloc + 1
            elemIds(iloc) = i
            elemCoords((iloc-1)*2+1) = lonCell(i)
            elemCoords((iloc-1)*2+2) = latCell(i)
            elemTypes(iloc) = nEdgesOnCell(i)
            numElemsConn = numElemsConn + nEdgesOnCell(i)
         end if
      end do
      ! print *, 'iloc, nCells_on_pet ', iloc, nCells_on_pet
      ASSERT(iloc == nCells_on_pet)
      ! print *, 'numElemsConn = ', numElemsConn

      ! define nodeIds, nodeCoords and nodeOwners
      nVertex_on_pet = 0
      do i =1, nVertices
         do j = 1, vertexDegree
            if (cellsOnVertex(j,i) > 0) then
               if (localpet == part_ids(cellsOnVertex(j,i))) then
                  nVertex_on_pet = nVertex_on_pet + 1
                  goto 90
               end if
            end if
         end do
90       continue
      end do
      ! print *, 'nVertex_on_pet = ', nVertex_on_pet

      numNodes = nVertex_on_pet
      allocate(nodeIds(numNodes))
      allocate(nodeCoords(2*numNodes))
      allocate(nodeOwners(numNodes))
      allocate(locNodeID(nVertices))
      locNodeID = 0
      iloc = 0
      do i =1,nVertices
         do j = 1, vertexDegree
            if (cellsOnVertex(j,i) > 0) then
               if (localpet == part_ids(cellsOnVertex(j,i))) then
                  iloc = iloc + 1
                  nodeIds(iloc) = i
                  nodeCoords((iloc-1)*2+1) = lonVertex(i)
                  nodeCoords((iloc-1)*2+2) = latVertex(i)
                  mv = huge(0)
                  do k = 1, vertexDegree
                     if (cellsOnVertex(k,i) > 0) then
                        mv = min(part_ids(cellsOnVertex(k,i)), mv)
                     end if
                  end do
                  nodeOwners(iloc) = mv
                  locNodeID(i) = iloc
                  goto 92
               end if
            end if
         end do
92       continue
      end do
      ! print *, 'iloc, nVertex_on_pet ', iloc, nVertex_on_pet
      ASSERT(iloc == nVertex_on_pet)

      allocate(elemConn(numElemsConn))
      ielemConn = 0
      do i =1,nCells
         if (localpet == part_ids(i)) then
            do j = 1, nEdgesOnCell(i)
               ielemConn = ielemConn + 1
               ASSERT (locNodeID(verticesOnCell(j,i)) > 0)
               elemConn(ielemConn) = locNodeID(verticesOnCell(j,i))
            end do
         end if
      end do
      ASSERT(ielemConn == numElemsConn)

      mesh = ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
                             coordSys=ESMF_COORDSYS_SPH_RAD, &
                             nodeIds=nodeIds, &
                             nodeCoords=nodeCoords, &
                             nodeOwners=nodeOwners, &
                             elementIds=elemIds, &
                             elementTypes=elemTypes, &
                             elementConn=elemConn, &
                             elementCoords=elemCoords, &
                             rc=rc); ESMF_ERR(rc)

   end subroutine create_mpas_esmf_mesh_from_file

end module mpas_esmf_mesh
