/*
 * Parallel Port LCD/LED Display Driver
 * Dylan Simon
 * 2001-04-21
 */

#include <linux/kernel.h>
#include <linux/version.h>
#ifdef CONFIG_MODVERSIONS
# include <linux/modversions.h>
#endif
#include <linux/module.h>
#include <linux/parport.h>
#include <asm/uaccess.h>
#include <linux/fs.h>
#include <linux/miscdevice.h>

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0))
#define OLD_LINUX_MODULES
#endif

#include "ppldd.h"

MODULE_AUTHOR("Dylan Simon <dylan@dylex.net>");
MODULE_DESCRIPTION("Parallel port LCD/LED display driver");
MODULE_LICENSE("unreleased");
MODULE_SUPPORTED_DEVICE("ppldd");
#ifdef OLD_LINUX_MODULES
EXPORT_NO_SYMBOLS;
#endif

int port = 0;
MODULE_PARM(port, "i");
MODULE_PARM_DESC(port, "parallel port number (0)");

struct pardevice *pdev;
struct parport *pport;

static ppldd_led_stat_t ledstat;
#ifdef PPLDD_ENABLE_LCD
static ppldd_lcd_disp_t lcddisp;
#endif

#define LED(n)		((unsigned char)(1 << ((n)-1)))
#define LEDON(n)	(ledstat |= LED(n))
#define LEDOFF(n)	(ledstat &= ~LED(n))
#define LEDTOG(n)	(ledstat ^= LED(n))
#define LEDSTAT(n)	(ledstat & LED(n))

void update_leds(void)
{
	parport_write_data(pport, ledstat);
}

#ifdef PPLDD_ENABLE_LCD
void update_lcd(void)
{
}
#endif

/* Get some information (text) */
ssize_t info_read(struct file *file, char *data, size_t size, loff_t *off)
{
	static char mydata[PPLDD_INFO_SIZE+1];
	char *p = mydata;
	int i, s;

	if (*off >= PPLDD_INFO_SIZE)
	{
		*off = PPLDD_INFO_SIZE;
		return 0;
	}

	for (i=1;i<1<<PPLDD_LED_COUNT;i<<=1)
	{
		*(p++)=(ledstat&i)?'1':'0';
	}
	*(p++) = '\n';

#ifdef PPLDD_ENABLE_LCD
	for (i = 0; i < PPLDD_LCD_ROWS; i++)
	{
		memcpy(p, lcddisp[i], PPLDD_LCD_COLS);
		p+=PPLDD_LCD_COLS;
		*(p++)='\n';
	}
#endif
	*p=0;

	if (*off+size < PPLDD_INFO_SIZE)
		s = size;
	else
		s = PPLDD_INFO_SIZE-*off;

	*off += s;
	if (copy_to_user(data, mydata, s))
		return -EFAULT;
	return s;
}

/* std ops */
int dev_open(struct inode *inode, struct file *file)
{
#ifdef OLD_LINUX_MODULES
	MOD_INC_USE_COUNT;
#endif
	return 0;
}
int dev_close(struct inode *inode, struct file *file)
{
#ifdef OLD_LINUX_MODULES
	MOD_DEC_USE_COUNT;
#endif
	return 0;
}

/* real work */
int dev_ioctl(struct inode *inode, struct file *file, unsigned int cmd, unsigned long arg)
{
#ifdef PPLDD_ENABLE_LCD
	union {
		struct ppldd_lcd_char chr;
		struct ppldd_lcd_data dat;
	} data;
#endif

	switch (cmd) 
	{
		case PPLDD_IOC_LED_GET:
			return put_user(ledstat, (ppldd_led_stat_t *)arg);

		case PPLDD_IOC_LED_SET:
			ledstat = (ppldd_led_stat_t)arg;
			update_leds();
			return 0;
			
		case PPLDD_IOC_LED_ON:
			LEDON((int)arg);
			update_leds();
			return 0;

		case PPLDD_IOC_LED_OFF:
			LEDOFF((int)arg);
			update_leds();
			return 0;

		case PPLDD_IOC_LED_TOG:
			LEDTOG((int)arg);
			update_leds();
			return 0;

#ifdef PPLDD_ENABLE_LCD
		case PPLDD_IOC_LCD_GET:
			if (copy_to_user((ppldd_lcd_disp_t *)arg, lcddisp, sizeof(ppldd_lcd_disp_t)))
				return -EFAULT;
			return 0;
			
		case PPLDD_IOC_LCD_SET:
			if (copy_from_user(lcddisp, (ppldd_lcd_disp_t *)arg, sizeof(ppldd_lcd_disp_t)))
				return -EFAULT;
			return 0;

		case PPLDD_IOC_LCD_CLEAR:
			memset(lcddisp, PPLDD_LCD_CHR_BLANK, sizeof(ppldd_lcd_disp_t));
			return 0;

		case PPLDD_IOC_LCD_SETCHAR:
			if (copy_from_user(&data.chr, (struct ppldd_lcd_char *)arg, sizeof(struct ppldd_lcd_char)))
				return -EFAULT;
			if (0 > data.chr.row || data.chr.row >= PPLDD_LCD_ROWS
			||  0 > data.chr.col || data.chr.col >= PPLDD_LCD_COLS)
				return -EINVAL;
			lcddisp[data.chr.row][data.chr.col] = data.chr.data;
			return 0;

		case PPLDD_IOC_LCD_WRITE:
			if (copy_from_user(&data.dat, (struct ppldd_lcd_data *)arg, sizeof(struct ppldd_lcd_data)))
				return -EFAULT;
			if (0 > data.dat.row || data.dat.row >= PPLDD_LCD_ROWS
			||  0 > data.dat.col || data.dat.col >= PPLDD_LCD_COLS
			||  0 >= data.dat.len || data.dat.len + data.dat.col > PPLDD_LCD_COLS)
				return -EINVAL;
			if (copy_from_user(data.dat.data, ((struct ppldd_lcd_data *)arg)->data, data.dat.len))
				return -EFAULT;
			memcpy(&(lcddisp[data.dat.row][data.dat.col]), data.dat.data, data.dat.len);
			return 0;
#endif

#ifdef OLD_LINUX_MODULES
		case PPLDD_IOC_UNUSE:
			MOD_DEC_USE_COUNT; /* hey, it happens */
			return 0;
#endif
	}
	return -ENOSYS;
}

struct file_operations devfops = {
	.owner =	THIS_MODULE,
	.read =		info_read,
	.ioctl =	dev_ioctl,
	.open =		dev_open,
	.release =	dev_close
};

struct miscdevice miscdev = {
	.minor =	PPLDD_MINOR,
	.name =		PPLDD_DEVICE,
	.fops =		&devfops,
	.devfs_name =	PPLDD_DEVICE
};

int init_module() 
{
	int err;

	if (!(pport = parport_find_number(port)))
	{
		printk(KERN_ERR "ppldd: no port %d\n", port);
		return -ENXIO;
	}

	pdev = parport_register_device(pport, "ppldd", NULL, NULL, NULL, PARPORT_DEV_EXCL, NULL);
	parport_put_port(pport);
	if (!pdev)
	{
		
		printk(KERN_ERR "ppldd: can't register device\n");
		return -ENXIO;
	}

	if ((err = parport_claim(pdev)))
	{
		printk(KERN_ERR "ppldd: device busy\n");
		parport_unregister_device(pdev);
		return err < 0 ? err : -EBUSY;
	}

	pport = pdev->port; /* probably unnecessarily futile */

#ifdef PPLDD_ENABLE_LCD
	memset(lcddisp, PPLDD_LCD_CHR_BLANK, sizeof(ppldd_lcd_disp_t));
#endif
	ledstat = 0;
	update_leds();
	if ((err = misc_register(&miscdev)))
	{
		printk(KERN_ERR "ppldd: couldn't register device: %d\n", err);
		parport_release(pdev);
		parport_unregister_device(pdev);
		return err;
	}

	return 0;
}

void cleanup_module()
{
	int err;

	if ((err = misc_deregister(&miscdev)))
	{
		printk(KERN_WARNING "ppldd: couldn't unregister device: %d\n", err);
	}
	ledstat = 0;
	update_leds();

	if (pdev) /* this can't be false */
	{
		parport_release(pdev);
		parport_unregister_device(pdev);
	}
}

