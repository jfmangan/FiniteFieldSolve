#include "WolframLibrary.h"
#include "WolframSparseLibrary.h"
#include "WolframNumericArrayLibrary.h"
#include <cstdint>

#if !ALLOC_STATIC_MEM
    #include <vector>
#endif

typedef U_INT_SHORT ushort_t;
typedef U_INT_LONG ulong_t;
typedef uint32_t index_t;

#if defined(_OPENMP)
    #include <omp.h>//Needed for open MP
#endif

//Boilerplate nonsense
EXTERN_C DLLEXPORT mint WolframLibrary_getVersion()
{
    return WolframLibraryVersion;
}

//More boilerplate nonsense
EXTERN_C DLLEXPORT void WolframLibrary_uninitialize( WolframLibraryData libData)
{
    return;
}

//It is just as performant to declare prime inside main.  The compiler figures everything out.  You have to modify ModP and FermatInverse if prime is inside main.
static const ulong_t prime=PRIME;//The largest prime that fits in 16bits is 65521.
//Mersenne primes don't improve performance likely due to a memory bandwidth bottleneck.
//It's critical for performance that the prime be const

//(row,col)=(i,j)
//It is just as performant to declare mat as a vector of vectors inside main
static const index_t NumRow=NUM_ROW;//const is static by default
static const index_t NumCol=NUM_COL;

#if ALLOC_STATIC_MEM
    static ushort_t mat[NumRow][NumCol];//The static keyword might not be needed but it's not hurting anything
#else
    std::vector< std::vector<ushort_t> >mat(NumRow, std::vector<ushort_t>(NumCol));
#endif

//Define the mod p function
//KACTL modmull for 64-bit primes?
inline ulong_t ModP(ulong_t input){
    return input%prime;
}

//Calculate the multiplicative inverse of input using Fermat's little theorem
ulong_t FermatInverse(ulong_t input){
    ulong_t prod = 1;
    ulong_t pow=prime-2;
    
    while (pow > 0){
        // If last bit of pow is 1, multiply prod by input
        if (pow & 1) prod = ModP(prod*input);
        pow = pow>>1;//Go to next bit
        input = ModP(input*input);
    }
    return prod;
}

EXTERN_C DLLEXPORT int PopulateRowOfMatrix(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument res) {
    ushort_t *NumArrData = NULL;
    WolframNumericArrayLibrary_Functions NumArrFuncs = libData->numericarrayLibraryFunctions;
    
    MNumericArray NumArr = MArgument_getMNumericArray(Args[0]);
    
    int err = LIBRARY_FUNCTION_ERROR;
    if(NumArrFuncs->MNumericArray_getRank(NumArr) != 1)
        return err;
    if(NumArrFuncs->MNumericArray_getFlattenedLength(NumArr) != NumCol)
        return err;
    if((NumArrFuncs->MNumericArray_getType(NumArr) != MNumericArray_Type_UBit16) && (NumArrFuncs->MNumericArray_getType(NumArr) != MNumericArray_Type_UBit32))
        return err;
    
    NumArrData = static_cast<ushort_t *>(NumArrFuncs->MNumericArray_getData(NumArr));
    
    mint i = MArgument_getInteger(Args[1])-1;//MMA starts indexing at 1 while CPP starts indexing at 0
    
    //Copy data into mat
    for(index_t j=0; j<NumCol; j++){
        mat[i][j]=NumArrData[j];
    }
    
    MArgument_setInteger(res, 0);
    return LIBRARY_NO_ERROR;
}


EXTERN_C DLLEXPORT int RowReduceNumericArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument res) {
    
    
    //RREF code
    
    for(index_t iRow=0; iRow<NumRow; iRow++){
        
        //Find pivot col
        bool FoundPivot=false;
        index_t jPivot;
        for(index_t j=0; j<NumCol; j++){//Rather than starting at j=0 you can keep track of the first potentially non-zero pivot col and start the loop there.  This would cut down on the range of the for loop.  However it doesn't make any impact on performance in testing and it makes the code uglier.
            if(mat[iRow][j] != 0){//Worry about floats here
                jPivot=j;
                FoundPivot=true;
                break;
            }
        }
        
        if(FoundPivot == false) continue;//If there's no non-zero entry in the row go to the next row
        
        //Normalize the row
        ulong_t InversePivotVal=FermatInverse(mat[iRow][jPivot]);
        for(index_t j=jPivot; j<NumCol; j++) mat[iRow][j]=ModP(ulong_t(mat[iRow][j])*InversePivotVal);
        
        //Subtract off the pivot row from everyone else
        #pragma omp parallel for
        for(index_t i=0; i<NumRow; i++){
            //Worry about the next line for parallelization
            //The if statement could be removed by changing the loop bounds but that would result in much uglier code
            if(i==iRow) continue;//Don't subtract the pivot row from itself
                
            ulong_t mult=mat[i][jPivot];
            if(mult==0) continue;//If mult is already zero then there's nothing to do on this row
            mat[i][jPivot]=0;
            for(index_t j=jPivot+1; j<NumCol; j++){//You only need to subtract off entries after the pivot column because jPivot is the first non-zero entry in the pivot row
                ulong_t tmp=ModP(mult*ulong_t(mat[iRow][j]));//This is probably the most expensive line in the whole program
                if(tmp>mat[i][j]) mat[i][j]=mat[i][j]+prime-tmp;
                else mat[i][j]=mat[i][j]-tmp;
            }
        }
    }
    
    
    //Convert mat to a SparseArray and return it
    
    WolframSparseLibrary_Functions sparseFuns = libData->sparseLibraryFunctions;
    
    mint NumNonZeroPos=0;
    for(index_t i=0; i<NumRow; i++){
        for(index_t j=0; j<NumCol; j++){
            if(mat[i][j]!=0) NumNonZeroPos++;
        }
    }
    
    const mint PosTensorDim[] = {NumNonZeroPos,2};
    const mint ValsTensorDim[] = {NumNonZeroPos};
    MTensor PosTensor, ValsTensor;
    libData->MTensor_new(MType_Integer, 2, PosTensorDim, &PosTensor);//Should probably check if there's memory to allocate
    libData->MTensor_new(MType_Integer, 1, ValsTensorDim, &ValsTensor);
    mint PosCounter[]={1,1}, ValsCounter=1;
    
    for(index_t i=0; i<NumRow; i++){
        for(index_t j=0; j<NumCol; j++){
            if(mat[i][j]==0) continue;
            libData->MTensor_setInteger(PosTensor, PosCounter, i+1);//CPP starts indexing at 0 and MMA starts indexing at 1
            PosCounter[1]++;
            libData->MTensor_setInteger(PosTensor, PosCounter, j+1);
            PosCounter[0]++;
            PosCounter[1]=1;
            libData->MTensor_setInteger(ValsTensor, &ValsCounter, mat[i][j]);
            ValsCounter++;
        }
    }
    
    mint RetDim[]={2};
    MTensor RetDimTensor;
    libData->MTensor_new(MType_Integer, 1, RetDim, &RetDimTensor);
    RetDim[0]=1;
    libData->MTensor_setInteger(RetDimTensor, RetDim, NumRow);
    RetDim[0]=2;
    libData->MTensor_setInteger(RetDimTensor, RetDim, NumCol);
    MSparseArray RetSparseArr = 0;
    (*(sparseFuns->MSparseArray_fromExplicitPositions))(PosTensor, ValsTensor, RetDimTensor, 0, &RetSparseArr);
    
    libData->MTensor_free(PosTensor);
    libData->MTensor_free(ValsTensor);
    libData->MTensor_free(RetDimTensor);
    
    MArgument_setMSparseArray(res, RetSparseArr);
    return LIBRARY_NO_ERROR;
}
