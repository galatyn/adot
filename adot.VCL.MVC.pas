unit adot.VCL.MVC;

interface

uses
  Vcl.Forms,
  System.Classes;

type
  {
    Example:
    *******************************************************************************
    Sets.Model.pas / Sets.Model.dfm
      TSetsDM = class(TDataModule, IModel)
        ...
      end;

      var
        SetsDM: TSetsDM;

    *******************************************************************************
    Sets.View.pas / Sets.View.dfm
      TFormSets = class(TForm, IView)
        ...
      end;

    *******************************************************************************
    Sets.Controller.Pas
      TController = class(TCustomController)
      private
        FModelObj: TSetsDM;
        FViewObj: TFormSets;
        FModel: IModel;
        FView: IView;
      end;

      constructor TController.Create;
      begin
        MVC.CreateModelAndView<TSetsDM, TFormSets>(
          SetsDM,    // automatic global var in data module
          FModelObj, // field of type TSetsDM
          FViewObj   // field of type TFormSets
        );
        FModel := FModelObj;         // field of type IModel
        FView  := FViewObj;          // field of type IView
        FModel.SetController(Self);
        FView.SetController(Self);
      end;

      destructor TController.Destroy;
      begin
        FView  := nil;             // release interface first
        FModel := nil;             // release interface first
        Sys.FreeAndNil(FViewObj);  // then we can release view  (in Delphi a form is using data module)
        Sys.FreeAndNil(FModelObj); // then we can release model (it is not in use anymore)
        inherited;
      end;
    *******************************************************************************
  }
  MVC = class
  public

    {
      Creates form & private instance of data module. Usefull for MVC pattern.
      Delphi by default creates one shared global instance of data unit when app is initializing.
    }
    class procedure CreateModelAndView<TModel: TDataModule; TView: TForm>(
      var ADMGlobalVar : TModel; { when form is creating, it is important to assign correct instance of DM to global var }
      out ADataModule  : TModel; { new instance of a data module }
      out AForm        : TView;  { new instance of a form }
          ADMOwner     : TComponent;
          AFormOwner   : TComponent
    ); overload; static;

    {
      Same, but with no owner.
    }
    class procedure CreateModelAndView<TModel: TDataModule; TView: TForm>(
      var ADMGlobalVar : TModel; { when form is creating, it is important to assign correct instance of DM to global var }
      out ADataModule  : TModel; { new instance of a data module }
      out AForm        : TView   { new instance of a form }
    ); overload; static;
  end;

implementation

class procedure MVC.CreateModelAndView<TModel, TView>(
  var ADMGlobalVar : TModel;
  out ADataModule  : TModel;
  out AForm        : TView;
      ADMOwner     : TComponent;
      AFormOwner   : TComponent);
var
  PrevDMInst: TModel;
begin
  PrevDMInst := ADMGlobalVar;
  try
    ADataModule  := TModel.Create(ADMOwner);
    ADMGlobalVar := ADMGlobalVar; { should be assigned when form is creating }
    AForm        := TView.Create(AFormOwner);
  finally
    ADMGlobalVar := PrevDMInst;
  end;
end;

class procedure MVC.CreateModelAndView<TModel, TView>(var ADMGlobalVar: TModel; out ADataModule: TModel; out AForm: TView);
begin
  CreateModelAndView<TModel, TView>(ADMGlobalVar, ADataModule, AForm, nil, nil);
end;

end.
