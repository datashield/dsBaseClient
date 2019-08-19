#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.listClientsideFunctions::smk::check results")
test_that("check results", {
    output <- paste(
  [1] "ds.test_env"                         
  [2] "checkClass"                          
  [3] "colPercent"                          
  [4] "ds.asCharacter"                      
  [5] "ds.asDataMatrix"                     
  [6] "ds.asFactor"                         
  [7] "ds.asInteger"                        
  [8] "ds.asList"                           
  [9] "ds.asLogical"                        
 [10] "ds.asMatrix"                         
 [11] "ds.asNumeric"                        
 [12] "ds.assign"                           
 [13] "ds.Boole"                            
 [14] "ds.c"                                
 [15] "ds.cbind"                            
 [16] "ds.changeRefGroup"                   
 [17] "ds.class"                            
 [18] "ds.colnames"                         
 [19] "ds.contourPlot"                      
 [20] "ds.cor"                              
 [21] "ds.corTest"                          
 [22] "ds.cov"                              
 [23] "ds.dataFrame"                        
 [24] "ds.dataFrameSort"                    
 [25] "ds.dataFrameSubset"                  
 [26] "ds.densityGrid"                      
 [27] "ds.dim"                              
 [28] "ds.exists"                           
 [29] "ds.exp"                              
 [30] "ds.gee"                              
 [31] "ds.glm"                              
 [32] "ds.glmSLMA"                          
 [33] "ds.heatmapPlot"                      
 [34] "ds.histogram"                        
 [35] "ds.isNA"                             
 [36] "ds.isValid"                          
 [37] "ds.length"                           
 [38] "ds.levels"                           
 [39] "ds.lexis"                            
 [40] "ds.list"                             
 [41] "ds.listClientsideFunctions"          
 [42] "ds.listDisclosureSettings"           
 [43] "ds.listOpals"                        
 [44] "ds.listServersideFunctions"          
 [45] "ds.log"                              
 [46] "ds.look"                             
 [47] "ds.ls"                               
 [48] "ds.make"                             
 [49] "ds.matrix"                           
 [50] "ds.matrixDet"                        
 [51] "ds.matrixDet.report"                 
 [52] "ds.matrixDiag"                       
 [53] "ds.matrixDimnames"                   
 [54] "ds.matrixInvert"                     
 [55] "ds.matrixMult"                       
 [56] "ds.matrixTranspose"                  
 [57] "ds.mean"                             
 [58] "ds.meanByClass"                      
 [59] "ds.meanSdGp"                         
 [60] "ds.merge"                            
 [61] "ds.message"                          
 [62] "ds.names"                            
 [63] "ds.numNA"                            
 [64] "ds.quantileMean"                     
 [65] "ds.rbind"                            
 [66] "ds.rBinom"                           
 [67] "ds.recodeLevels"                     
 [68] "ds.recodeValues"                     
 [69] "ds.replaceNA"                        
 [70] "ds.reShape"                          
 [71] "ds.rm"                               
 [72] "ds.rNorm"                            
 [73] "ds.rowColCalc"                       
 [74] "ds.rPois"                            
 [75] "ds.rUnif"                            
 [76] "ds.scatterPlot"                      
 [77] "ds.seq"                              
 [78] "ds.setDefaultOpals"                  
 [79] "ds.setSeed"                          
 [80] "ds.subset"                           
 [81] "ds.subsetByClass"                    
 [82] "ds.summary"                          
 [83] "ds.table1D"                          
 [84] "ds.table2D"                          
 [85] "ds.tapply"                           
 [86] "ds.tapply.assign"                    
 [87] "ds.testObjExists"                    
 [88] "ds.unList"                           
 [89] "ds.var"                              
 [90] "ds.vectorCalc"                       
 [91] "extract"                             
 [92] "findLoginObjects"                    
 [93] "geeChecks"                           
 [94] "geehelper1"                          
 [95] "geehelper2"                          
 [96] "getOpals"                            
 [97] "getPooledMean"                       
 [98] "getPooledVar"                        
 [99] "glmChecks"                           
[100] "init.object.list.global.environment" 
[101] "init.object.list.testing.environment"
[102] "init.opal.list"                      
[103] "isAssigned"                          
[104] "isDefined"                           
[105] "library.dynam.unload"                
[106] "logical2int"                         
[107] "meanByClassHelper0a"                 
[108] "meanByClassHelper0b"                 
[109] "meanByClassHelper1"                  
[110] "meanByClassHelper2"                  
[111] "meanByClassHelper3"
[112] "meanByClassHelper4"
[113] "rowPercent"
[114] "subsetHelper"
[115] "system.file"
    )

    res <- ds.listClientsideFunctions()

    print("======")
    print(res)
    print("======")

    expect_equal(res, output)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
