#define PPLDD_DEVICE	"ppldd"
#define PPLDD_MAJOR	10	/* misc */
#define PPLDD_MINOR	244
#define PPLDD_IOC_MAGIC	'*'	/* not officially registered */
#ifdef PPLDD_ENABLE_LCD
#define PPLDD_LCD_ROWS	4
#define PPLDD_LCD_COLS	20
#endif
#define PPLDD_LED_COUNT	8	/* we explicitly use uchars anyway */
#ifdef PPLDD_ENABLE_LCD
#define PPLDD_INFO_SIZE	(PPLDD_LED_COUNT+1 + PPLDD_LCD_ROWS*(PPLDD_LCD_COLS+1))
#else
#define PPLDD_INFO_SIZE	(PPLDD_LED_COUNT+1)
#endif

typedef unsigned char ppldd_led_stat_t;
#ifdef PPLDD_ENABLE_LCD
typedef char ppldd_lcd_disp_t[PPLDD_LCD_ROWS][PPLDD_LCD_COLS];
struct ppldd_lcd_char {
	int row, col;
	char data;
};
struct ppldd_lcd_data {
	int row, col, len;
	char data[PPLDD_LCD_COLS];
};
#endif

#define PPLDD_IOC_LED_GET	_IOR (PPLDD_IOC_MAGIC, 0, ppldd_led_stat_t)
#define PPLDD_IOC_LED_SET	_IOW (PPLDD_IOC_MAGIC, 1, ppldd_led_stat_t)
#define PPLDD_IOC_LED_ON	_IOW (PPLDD_IOC_MAGIC, 2, int)
#define PPLDD_IOC_LED_OFF	_IOW (PPLDD_IOC_MAGIC, 3, int)
#define PPLDD_IOC_LED_TOG	_IOW (PPLDD_IOC_MAGIC, 4, int)

#ifdef PPLDD_ENABLE_LCD
#define PPLDD_IOC_LCD_GET	_IOR (PPLDD_IOC_MAGIC, 16, ppldd_lcd_disp_t)
#define PPLDD_IOC_LCD_SET	_IOW (PPLDD_IOC_MAGIC, 17, ppldd_lcd_disp_t)
#define PPLDD_IOC_LCD_CLEAR	_IO  (PPLDD_IOC_MAGIC, 18)
#define PPLDD_IOC_LCD_SETCHAR	_IOW (PPLDD_IOC_MAGIC, 19, struct ppldd_lcd_char)
#define PPLDD_IOC_LCD_WRITE	_IOW (PPLDD_IOC_MAGIC, 20, struct ppldd_lcd_data)
#endif

#define PPLDD_IOC_UNUSE		_IO  (PPLDD_IOC_MAGIC, 128)

#ifdef PPLDD_ENABLE_LCD
#define PPLDD_LCD_CHR_BLANK	0x20
#endif
