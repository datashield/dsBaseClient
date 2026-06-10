#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------


# context("ds.table::math::row and col proportions::multiple")

test_that("row and col proportions sum to 1", 
          {
            connect.testing.group.dataset.1()
            
            server.result <- ds.table("D$COLOURS", "D$POSITIVE.NUMBERS")
            
            ##when 2 studies being combined for table: $output.list gets the tables part, 
            # [[1]] is study 1 row props
            # [[2]] is study 1 col props
            # [[3]] is study 2 row props
            # [[4]] is study 2 col props
            # [[5]] is combined study row props
            # [[6]] is combined study col props
            # [[7]] onwards is counts (study 1, study 2, combined)
            
            #colprops
            for(k in c(2,4,6)){
            for (j in 1:4) {
              study1_col_totals=0
              for (i in 1:5){
                study1_col_totals=study1_col_totals+ server.result[1]$output.list[[k]][[i,j]]

              }
              expect_equal(study1_col_totals,1,tolerance =2*10^-3)
            }
          }
            #rowprops
            for(k in c(1,3,5)){
            
            for (i in 1:5){
              study1_row_totals=0
              for (j in 1:4){
                study1_row_totals=study1_row_totals+server.result[1]$output.list[[k]][[i,j]]
              }
              expect_equal(study1_row_totals,1,tolerance =2*10^-3)
            }
            }
            
            
            disconnect.testing.group.dataset.1()
          }
)
