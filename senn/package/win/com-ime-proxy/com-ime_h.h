

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


 /* File created by MIDL compiler version 8.01.0622 */
/* at Tue Jan 19 12:14:07 2038
 */
/* Compiler settings for com-ime.idl:
    Oicf, W1, Zp8, env=Win64 (32b run), target_arch=AMD64 8.01.0622 
    protocol : all , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
/* @@MIDL_FILE_HEADING(  ) */



/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 500
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif /* __RPCNDR_H_VERSION__ */

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __com2Dime_h_h__
#define __com2Dime_h_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Forward Declarations */ 

#ifndef __ISennComIme_FWD_DEFINED__
#define __ISennComIme_FWD_DEFINED__
typedef interface ISennComIme ISennComIme;

#endif 	/* __ISennComIme_FWD_DEFINED__ */


#ifndef __Senn_FWD_DEFINED__
#define __Senn_FWD_DEFINED__

#ifdef __cplusplus
typedef class Senn Senn;
#else
typedef struct Senn Senn;
#endif /* __cplusplus */

#endif 	/* __Senn_FWD_DEFINED__ */


/* header files for imported files */
#include "oaidl.h"
#include "ocidl.h"

#ifdef __cplusplus
extern "C"{
#endif 


#ifndef __ISennComIme_INTERFACE_DEFINED__
#define __ISennComIme_INTERFACE_DEFINED__

/* interface ISennComIme */
/* [object][version][uuid] */ 


EXTERN_C const IID IID_ISennComIme;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("5F3E54F4-C755-4048-B601-562219BBE767")
    ISennComIme : public IUnknown
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE Request( 
            LPCSTR req,
            /* [out] */ LPSTR *res) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ISennComImeVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ISennComIme * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ISennComIme * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ISennComIme * This);
        
        HRESULT ( STDMETHODCALLTYPE *Request )( 
            ISennComIme * This,
            LPCSTR req,
            /* [out] */ LPSTR *res);
        
        END_INTERFACE
    } ISennComImeVtbl;

    interface ISennComIme
    {
        CONST_VTBL struct ISennComImeVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ISennComIme_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ISennComIme_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ISennComIme_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ISennComIme_Request(This,req,res)	\
    ( (This)->lpVtbl -> Request(This,req,res) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ISennComIme_INTERFACE_DEFINED__ */



#ifndef __SennLibrary_LIBRARY_DEFINED__
#define __SennLibrary_LIBRARY_DEFINED__

/* library SennLibrary */
/* [version][uuid] */ 


EXTERN_C const IID LIBID_SennLibrary;

EXTERN_C const CLSID CLSID_Senn;

#ifdef __cplusplus

class DECLSPEC_UUID("BB56A7D4-F240-4494-A081-354F4F2BA873")
Senn;
#endif
#endif /* __SennLibrary_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif


