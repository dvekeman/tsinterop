declare namespace isc {

  type HTMLString = string;

  class RPCRequest {}

  class DSRequest extends RPCRequest {}

  class FormItem {
    title: HTMLString;

    someValue: any;

    optionFilterContext: RPCRequest;
  }

  class SelectItem extends FormItem {
    title: HTMLString;

    someValue: string;

    optionFilterContext: DSRequest;
  }

}