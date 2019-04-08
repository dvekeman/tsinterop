declare namespace isc {

  type HTMLString = string;

  class FormItem {
    title: HTMLString;

  }

  class SelectItem extends FormItem {
    title: string;
  }

}