#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <raylib.h>

CAMLprim value caml_init_window(value width, value height, value title)
{
    CAMLparam3(width, height, title);
    InitWindow(Int_val(width), Int_val(height), String_val(title));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_set_target_fps(value fps)
{
    CAMLparam1(fps);
    SetTargetFPS(Int_val(fps));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_window_should_close(value unit)
{
    CAMLparam1(unit);
    bool result = WindowShouldClose();
    CAMLreturn(Val_bool(result));
}

CAMLprim value caml_begin_drawing(value unit)
{
    CAMLparam1(unit);
    BeginDrawing();
    CAMLreturn(Val_unit);
}

CAMLprim value caml_end_drawing(value unit)
{
    CAMLparam1(unit);
    EndDrawing();
    CAMLreturn(Val_unit);
}

Color color_of_value(value v)
{
    Color c = {0};
    c.r = Int_val(Field(v, 0));
    c.g = Int_val(Field(v, 1));
    c.b = Int_val(Field(v, 2));
    c.a = Int_val(Field(v, 3));
    return c;
}

Vector2 vector2_of_value(value v)
{
    Vector2 vec2 = {0};
    vec2.x = Double_field(v, 0);
    vec2.y = Double_field(v, 1);
    return vec2;
}

Rectangle rectangle_of_value(value v)
{
    Rectangle rec = {0};
    rec.x      = Double_field(v, 0);
    rec.y      = Double_field(v, 1);
    rec.width  = Double_field(v, 2);
    rec.height = Double_field(v, 3);
    return rec;
}

CAMLprim value caml_clear_background(value color)
{
    CAMLparam1(color);
    ClearBackground(color_of_value(color));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_close_window(value unit)
{
    CAMLparam1(unit);
    CloseWindow();
    CAMLreturn(Val_unit);
}

CAMLprim value caml_draw_rectangle(value x, value y, value w, value h, value color)
{
    CAMLparam5(x, y, w, h, color);
    DrawRectangle(Int_val(x), Int_val(y), Int_val(w), Int_val(h), color_of_value(color));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_get_render_width(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(GetRenderWidth()));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_get_render_height(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(GetRenderHeight()));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_is_key_pressed(value key)
{
    CAMLparam1(key);
    CAMLreturn(Val_bool(IsKeyPressed(Int_val(key))));
}

CAMLprim value caml_is_key_down(value key)
{
    CAMLparam1(key);
    CAMLreturn(Val_bool(IsKeyDown(Int_val(key))));
}

CAMLprim value caml_draw_circle(value x, value y, value radius, value color)
{
    CAMLparam4(x, y, radius, color);
    DrawCircle(Int_val(x), Int_val(y), Double_val(radius), color_of_value(color));
    CAMLreturn(Val_unit);
}

CAMLprim value caml_draw_rectangle_pro(value rec, value origin, value rotation, value color)
{
    CAMLparam4(rec, origin, rotation, color);
    DrawRectanglePro(
        rectangle_of_value(rec),
        vector2_of_value(origin),
        Double_val(rotation),
        color_of_value(color));
    CAMLreturn(Val_unit);
}
